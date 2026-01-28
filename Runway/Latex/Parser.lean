/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Runway.Latex.Token
import Runway.Latex.Lexer
import Runway.Latex.Ast

/-!
# LaTeX Parser

A parser for LaTeX documents that produces an AST.
-/

namespace Runway.Latex

/-- Parser state -/
structure ParserState where
  tokens : Array Token
  pos : Nat
  errors : Array String
  deriving Repr, Inhabited

/-- Parser monad -/
abbrev ParserM := StateM ParserState

namespace Parser

/-- Check if at end of tokens -/
def isEof : ParserM Bool := do
  let s ← get
  return s.pos >= s.tokens.size

/-- Peek current token -/
def peek : ParserM (Option Token) := do
  let s ← get
  if s.pos >= s.tokens.size then return none
  else return some s.tokens[s.pos]!

/-- Peek current token kind -/
def peekKind : ParserM (Option TokenKind) := do
  match ← peek with
  | some tok => return some tok.kind
  | none => return none

/-- Consume and return current token -/
def advance : ParserM (Option Token) := do
  let s ← get
  if s.pos >= s.tokens.size then return none
  else
    let tok := s.tokens[s.pos]!
    set { s with pos := s.pos + 1 }
    return some tok

/-- Add error -/
def addError (msg : String) : ParserM Unit := do
  let s ← get
  set { s with errors := s.errors.push msg }

/-- Skip whitespace and newlines -/
def skipWhitespace : ParserM Unit := do
  while true do
    match ← peekKind with
    | some (.whitespace _) | some .newline => let _ ← advance
    | _ => break

/-- Skip whitespace, newlines, and comments -/
def skipTrivia : ParserM Unit := do
  while true do
    match ← peekKind with
    | some (.whitespace _) | some .newline | some (.comment _) => let _ ← advance
    | _ => break

/-- Check if current is specific command -/
def isCommand (name : String) : ParserM Bool := do
  match ← peekKind with
  | some (.command n) => return n == name
  | _ => return false

/-- Parse brace content -/
def parseBraceContent : ParserM String := do
  skipWhitespace
  match ← peekKind with
  | some .braceOpen =>
    let _ ← advance
    let mut content := ""
    let mut depth := 1
    while depth > 0 do
      match ← advance with
      | some tok =>
        match tok.kind with
        | .braceOpen => depth := depth + 1; content := content ++ "{"
        | .braceClose => depth := depth - 1; if depth > 0 then content := content ++ "}"
        | _ => content := content ++ tok.raw
      | none => break
    return content.trimAscii.toString
  | _ => return ""

/-- Parse optional bracket content -/
def parseBracketContent : ParserM (Option String) := do
  skipWhitespace
  match ← peekKind with
  | some .bracketOpen =>
    let _ ← advance
    let mut content := ""
    let mut depth := 1
    while depth > 0 do
      match ← advance with
      | some tok =>
        match tok.kind with
        | .bracketOpen => depth := depth + 1; content := content ++ "["
        | .bracketClose => depth := depth - 1; if depth > 0 then content := content ++ "]"
        | _ => content := content ++ tok.raw
      | none => break
    return some content.trimAscii.toString
  | _ => return none

/-- Parse comma-separated list -/
def parseCommaSeparated (s : String) : Array String :=
  s.splitOn "," |>.map (fun x => x.trimAscii.toString) |>.filter (!·.isEmpty) |>.toArray

/-- Parse Lean names -/
def parseLeanNames (s : String) : Array Lean.Name :=
  parseCommaSeparated s |>.map String.toName

/-- Parse theorem metadata -/
def parseTheoremMeta : ParserM TheoremMetadata := do
  let mut md : TheoremMetadata := {}
  while true do
    skipTrivia
    match ← peekKind with
    | some (.command name) =>
      match name with
      | "label" =>
        let _ ← advance
        md := { md with label := some (← parseBraceContent) }
      | "lean" =>
        let _ ← advance
        md := { md with leanDecls := md.leanDecls ++ parseLeanNames (← parseBraceContent) }
      | "uses" =>
        let _ ← advance
        md := { md with uses := md.uses ++ parseCommaSeparated (← parseBraceContent) }
      | "proves" =>
        let _ ← advance
        md := { md with proves := some (← parseBraceContent) }
      | "leanok" =>
        let _ ← advance
        md := { md with leanOk := true }
      | "notready" =>
        let _ ← advance
        md := { md with notReady := true }
      | "mathlibok" =>
        let _ ← advance
        md := { md with leanOk := true, mathLibOk := true }
      | "discussion" =>
        let _ ← advance
        md := { md with discussion := (← parseBraceContent).toNat? }
      | "leanposition" =>
        let _ ← advance
        md := { md with position := some (← parseBraceContent) }
      | "leansignaturesourcehtml" =>
        let _ ← advance
        md := { md with signatureHtml := some (← parseBraceContent) }
      | "leanproofsourcehtml" =>
        let _ ← advance
        md := { md with proofHtml := some (← parseBraceContent) }
      | "leanhoverdata" =>
        let _ ← advance
        md := { md with hoverData := some (← parseBraceContent) }
      | _ => break
    | _ => break
  return md

/-- Parse inline content -/
partial def parseInlines (stop : TokenKind → Bool) : ParserM (Array Inline) := do
  let mut inlines : Array Inline := #[]
  while true do
    match ← peekKind with
    | none => break
    | some kind =>
      if stop kind then break
      match kind with
      | .text content =>
        let _ ← advance
        inlines := inlines.push (Inline.text content)
      | .whitespace _ | .newline =>
        let _ ← advance
        inlines := inlines.push Inline.space
      | .mathInline content =>
        let _ ← advance
        inlines := inlines.push (Inline.math content)
      | .command name =>
        let _ ← advance
        match name with
        | "emph" | "textit" =>
          inlines := inlines.push (Inline.emph #[Inline.text (← parseBraceContent)])
        | "textbf" =>
          inlines := inlines.push (Inline.bold #[Inline.text (← parseBraceContent)])
        | "texttt" =>
          inlines := inlines.push (Inline.code (← parseBraceContent))
        | "ref" =>
          inlines := inlines.push (Inline.ref (← parseBraceContent))
        | "cite" =>
          inlines := inlines.push (Inline.cite (parseCommaSeparated (← parseBraceContent)))
        | "lean" =>
          inlines := inlines.push (Inline.lean (parseLeanNames (← parseBraceContent)))
        | _ =>
          inlines := inlines.push (Inline.raw ("\\" ++ name))
      | .comment _ =>
        let _ ← advance
      | _ => break
  return inlines

/-- Simple stop condition for paragraphs -/
def paragraphStop (k : TokenKind) : Bool :=
  match k with
  | .paragraph | .command "end" | .command "begin" | .command "section"
  | .command "subsection" | .command "chapter" => true
  | _ => false

/-- Helper to parse optional label -/
def parseOptionalLabel : ParserM (Option String) := do
  if ← isCommand "label" then
    let _ ← advance
    return some (← parseBraceContent)
  else
    return none

mutual

/-- Parse blocks until end of environment -/
partial def parseBlocks (envName : String) : ParserM (Array Block) := do
  let mut blocks : Array Block := #[]
  while true do
    skipTrivia
    match ← peekKind with
    | none => break
    | some (.command "end") =>
      let _ ← advance
      let endName ← parseBraceContent
      if endName == envName then break
      else do addError ("Mismatched \\end{" ++ endName ++ "}"); break
    | some (.command "begin") =>
      let _ ← advance
      let nestedName ← parseBraceContent
      let nested ← parseEnv nestedName
      blocks := blocks.push nested
    | some .paragraph =>
      let _ ← advance
    | some (.mathDisplay content) =>
      let _ ← advance
      blocks := blocks.push (Block.displayMath content)
    | _ =>
      let inlines ← parseInlines paragraphStop
      if !inlines.isEmpty then
        blocks := blocks.push (Block.paragraph inlines)
      else
        -- Avoid infinite loop: if parseInlines returns empty without advancing, skip the token
        let _ ← advance
  return blocks

/-- Parse an environment -/
partial def parseEnv (envName : String) : ParserM Block := do
  let _ ← parseBracketContent

  if isTheoremEnv envName then
    let md ← parseTheoremMeta
    let content ← parseBlocks envName
    return Block.theorem envName none md content
  else if envName == "proof" then
    let md ← parseTheoremMeta
    let content ← parseBlocks envName
    return Block.proof md content
  else if envName == "itemize" || envName == "enumerate" then
    let items ← parseItems envName
    if envName == "itemize" then
      return Block.itemize items
    else
      return Block.enumerate items
  else if envName == "document" then
    let content ← parseBody
    return Block.document {} content
  else
    let _ ← parseBlocks envName  -- skip content for unknown environments
    return Block.raw ("\\begin{" ++ envName ++ "}...\\end{" ++ envName ++ "}")

/-- Parse list items -/
partial def parseItems (envName : String) : ParserM (Array (Array Block)) := do
  let mut items : Array (Array Block) := #[]
  let mut currentItem : Array Block := #[]

  while true do
    skipTrivia
    match ← peekKind with
    | none => break
    | some (.command "end") =>
      let _ ← advance
      let endName ← parseBraceContent
      if !currentItem.isEmpty then items := items.push currentItem
      if endName != envName then addError ("Mismatched \\end{" ++ endName ++ "}")
      break
    | some (.command "item") =>
      let _ ← advance
      let _ ← parseBracketContent
      if !currentItem.isEmpty then items := items.push currentItem
      currentItem := #[]
    | some .paragraph =>
      let _ ← advance
    | some (.mathDisplay content) =>
      let _ ← advance
      currentItem := currentItem.push (Block.displayMath content)
    | _ =>
      let inlines ← parseInlines fun k =>
        match k with
        | .paragraph | .command "end" | .command "item" => true
        | _ => false
      if !inlines.isEmpty then
        currentItem := currentItem.push (Block.paragraph inlines)
      else
        -- Avoid infinite loop: if parseInlines returns empty without advancing, skip the token
        let _ ← advance

  return items

/-- Parse document body -/
partial def parseBody : ParserM (Array Block) := do
  let mut blocks : Array Block := #[]

  while true do
    skipTrivia
    match ← peekKind with
    | none => break
    | some .eof => break
    | some (.command "end") =>
      let _ ← advance
      let endName ← parseBraceContent
      if endName == "document" then break
    | some (.command name) =>
      let _ ← advance
      match name with
      | "chapter" =>
        let title ← parseBraceContent
        skipTrivia
        let label ← parseOptionalLabel
        let body ← parseSectionBody 0
        blocks := blocks.push (Block.chapter (Inline.text title) label body)
      | "section" =>
        let title ← parseBraceContent
        skipTrivia
        let label ← parseOptionalLabel
        let body ← parseSectionBody 1
        blocks := blocks.push (Block.section 1 (Inline.text title) label body)
      | "subsection" =>
        let title ← parseBraceContent
        skipTrivia
        let label ← parseOptionalLabel
        let body ← parseSectionBody 2
        blocks := blocks.push (Block.section 2 (Inline.text title) label body)
      | "begin" =>
        let envName ← parseBraceContent
        let envBlock ← parseEnv envName
        blocks := blocks.push envBlock
      | "inputleanmodule" =>
        let moduleName ← parseBraceContent
        blocks := blocks.push (Block.inputLeanModule moduleName.toName)
      | "inputleannode" =>
        let lbl ← parseBraceContent
        blocks := blocks.push (Block.inputLeanNode lbl)
      | "paperstatement" =>
        let label ← parseBraceContent
        blocks := blocks.push (Block.paperStatement label)
      | "paperfull" =>
        let label ← parseBraceContent
        blocks := blocks.push (Block.paperFull label)
      | "paperproof" =>
        let label ← parseBraceContent
        blocks := blocks.push (Block.paperProof label)
      | "input" =>
        let _ ← parseBraceContent
      | _ =>
        let _ ← parseBraceContent
    | some .paragraph =>
      let _ ← advance
    | some (.mathDisplay content) =>
      let _ ← advance
      blocks := blocks.push (Block.displayMath content)
    | _ =>
      let inlines ← parseInlines paragraphStop
      if !inlines.isEmpty then
        blocks := blocks.push (Block.paragraph inlines)
      else
        -- Avoid infinite loop: if parseInlines returns empty without advancing, skip the token
        let _ ← advance

  return blocks

/-- Parse section body -/
partial def parseSectionBody (level : Nat) : ParserM (Array Block) := do
  let mut blocks : Array Block := #[]

  while true do
    skipTrivia
    match ← peekKind with
    | none => break
    | some .eof => break
    | some (.command "end") => break
    | some (.command name) =>
      let isHigher := match name with
        | "chapter" => true  -- Always break on chapter to return to parseBody
        | "section" => level >= 1
        | "subsection" => level >= 2
        | _ => false
      if isHigher then break
      else
        let _ ← advance
        match name with
        | "begin" =>
          let envName ← parseBraceContent
          let envBlock ← parseEnv envName
          blocks := blocks.push envBlock
        | "inputleanmodule" =>
          blocks := blocks.push (Block.inputLeanModule (← parseBraceContent).toName)
        | "inputleannode" =>
          blocks := blocks.push (Block.inputLeanNode (← parseBraceContent))
        | "paperstatement" =>
          blocks := blocks.push (Block.paperStatement (← parseBraceContent))
        | "paperfull" =>
          blocks := blocks.push (Block.paperFull (← parseBraceContent))
        | "paperproof" =>
          blocks := blocks.push (Block.paperProof (← parseBraceContent))
        | "section" =>
          let title ← parseBraceContent
          skipTrivia
          let label ← parseOptionalLabel
          let body ← parseSectionBody 1
          blocks := blocks.push (Block.section 1 (Inline.text title) label body)
        | "subsection" =>
          let title ← parseBraceContent
          skipTrivia
          let label ← parseOptionalLabel
          let body ← parseSectionBody 2
          blocks := blocks.push (Block.section 2 (Inline.text title) label body)
        | "input" =>
          let _ ← parseBraceContent
        | _ =>
          let _ ← parseBraceContent
    | some .paragraph =>
      let _ ← advance
    | some (.mathDisplay content) =>
      let _ ← advance
      blocks := blocks.push (Block.displayMath content)
    | _ =>
      let inlines ← parseInlines paragraphStop
      if !inlines.isEmpty then
        blocks := blocks.push (Block.paragraph inlines)
      else
        -- Avoid infinite loop: if parseInlines returns empty without advancing, skip the token
        let _ ← advance

  return blocks

end

/-- Parse preamble -/
def parsePreamble : ParserM Preamble := do
  let mut preamble : Preamble := {}

  while true do
    skipTrivia
    match ← peekKind with
    | none => break
    | some (.command name) =>
      match name with
      | "documentclass" =>
        let _ ← advance
        let opts ← parseBracketContent
        let cls ← parseBraceContent
        preamble := { preamble with
          documentClass := cls
          classOptions := opts.map parseCommaSeparated |>.getD #[]
        }
      | "usepackage" =>
        let _ ← advance
        let opts ← parseBracketContent
        let pkg ← parseBraceContent
        preamble := { preamble with
          packages := preamble.packages.push (pkg, opts.map parseCommaSeparated |>.getD #[])
        }
      | "title" =>
        let _ ← advance
        preamble := { preamble with title := some (← parseBraceContent) }
      | "author" =>
        let _ ← advance
        preamble := { preamble with author := some (← parseBraceContent) }
      | "date" =>
        let _ ← advance
        preamble := { preamble with date := some (← parseBraceContent) }
      | "begin" => break
      | _ =>
        let _ ← advance
        let _ ← parseBraceContent
    | _ =>
      let _ ← advance

  return preamble

/-- Parse complete document -/
def parseDocument : ParserM Document := do
  let preamble ← parsePreamble

  skipTrivia
  if ← isCommand "begin" then
    let _ ← advance
    let envName ← parseBraceContent
    if envName == "document" then
      let body ← parseBody
      return { root := Block.document preamble body }
    else
      addError "Expected \\begin{document}"
      return { root := Block.document preamble #[] }
  else
    let body ← parseBody
    return { root := Block.document preamble body }

end Parser

/-- Parse LaTeX source -/
def parse (source : String) : Document × Array String :=
  let tokens := tokenize source
  let state : ParserState := { tokens, pos := 0, errors := #[] }
  let (doc, finalState) := Parser.parseDocument.run state
  (doc, finalState.errors)

/-- Parse LaTeX file -/
def parseFile (path : System.FilePath) : IO (Document × Array String) := do
  IO.println s!"[DEBUG] parseFile: reading {path}"
  (← IO.getStdout).flush
  let content ← IO.FS.readFile path
  IO.println s!"[DEBUG] parseFile: read {content.length} chars, tokenizing..."
  (← IO.getStdout).flush
  let tokens := tokenize content
  IO.println s!"[DEBUG] parseFile: got {tokens.size} tokens, parsing..."
  (← IO.getStdout).flush
  let state : ParserState := { tokens, pos := 0, errors := #[] }
  let (doc, finalState) := Parser.parseDocument.run state
  IO.println s!"[DEBUG] parseFile: parsing complete, {finalState.errors.size} errors"
  (← IO.getStdout).flush
  return ({ doc with sourcePath := some path }, finalState.errors)

end Runway.Latex
