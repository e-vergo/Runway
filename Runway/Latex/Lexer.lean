/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Runway.Latex.Token

/-!
# LaTeX Lexer

A lexer for LaTeX documents. Produces a stream of tokens from source text.
-/

namespace Runway.Latex

/-- Lexer state using byte positions -/
structure LexerState where
  /-- Source text -/
  source : String
  /-- Current byte position -/
  pos : Nat
  /-- Current line number (1-indexed) -/
  line : Nat
  /-- Current column number (1-indexed) -/
  column : Nat
  deriving Repr, Inhabited

/-- Lexer monad -/
abbrev LexerM := StateM LexerState

namespace Lexer

/-- Check if we've reached the end of input -/
def isEof : LexerM Bool := do
  let s ← get
  return s.pos >= s.source.utf8ByteSize

/-- Get character at byte position -/
private def getCharAt (source : String) (pos : Nat) : Option Char :=
  if pos >= source.utf8ByteSize then none
  else some (String.Pos.Raw.get source ⟨pos⟩)

/-- Peek at the current character without consuming it -/
def peek : LexerM (Option Char) := do
  let s ← get
  return getCharAt s.source s.pos

/-- Peek at a character at offset from current position -/
def peekAt (offset : Nat) : LexerM (Option Char) := do
  let s ← get
  -- Simple approximation: assume ASCII for offset calculation
  return getCharAt s.source (s.pos + offset)

/-- Consume one character and advance position -/
def advance : LexerM (Option Char) := do
  let s ← get
  match getCharAt s.source s.pos with
  | none => return none
  | some c =>
    let charSize := c.utf8Size
    if c == '\n' then
      set { s with pos := s.pos + charSize, line := s.line + 1, column := 1 }
    else
      set { s with pos := s.pos + charSize, column := s.column + 1 }
    return some c

/-- Get current position -/
def getPosition : LexerM Position := do
  let s ← get
  return { line := s.line, column := s.column, offset := s.pos }

/-- Check if a character starts a command name -/
def isCommandChar (c : Char) : Bool :=
  c.isAlpha || c == '@'

/-- Consume characters while predicate holds -/
def takeWhile (p : Char → Bool) : LexerM String := do
  let mut result := ""
  while true do
    match ← peek with
    | some c =>
      if p c then
        let _ ← advance
        result := result.push c
      else
        break
    | none => break
  return result

/-- Consume characters until predicate holds or EOF -/
def takeUntil (p : Char → Bool) : LexerM String := do
  takeWhile (!p ·)

/-- Lex a command name (after the backslash) -/
def lexCommandName : LexerM String := do
  match ← peek with
  | some c =>
    if isCommandChar c then
      takeWhile isCommandChar
    else
      let _ ← advance
      return c.toString
  | none => return ""

/-- Lex inline math $...$ -/
def lexInlineMath : LexerM String := do
  let mut content := ""
  while true do
    match ← peek with
    | some '$' =>
      let _ ← advance
      break
    | some '\\' =>
      let _ ← advance
      content := content.push '\\'
      match ← advance with
      | some c => content := content.push c
      | none => break
    | some c =>
      let _ ← advance
      content := content.push c
    | none => break
  return content

/-- Lex display math $$...$$ -/
def lexDisplayMathDollar : LexerM String := do
  let mut content := ""
  while true do
    match ← peek with
    | some '$' =>
      match ← peekAt 1 with
      | some '$' =>
        let _ ← advance
        let _ ← advance
        break
      | _ =>
        let _ ← advance
        content := content.push '$'
    | some '\\' =>
      let _ ← advance
      content := content.push '\\'
      match ← advance with
      | some c => content := content.push c
      | none => break
    | some c =>
      let _ ← advance
      content := content.push c
    | none => break
  return content

/-- Lex display math \[...\] -/
def lexDisplayMathBracket : LexerM String := do
  let mut content := ""
  while true do
    match ← peek with
    | some '\\' =>
      match ← peekAt 1 with
      | some ']' =>
        let _ ← advance
        let _ ← advance
        break
      | _ =>
        let _ ← advance
        content := content.push '\\'
        match ← advance with
        | some c => content := content.push c
        | none => break
    | some c =>
      let _ ← advance
      content := content.push c
    | none => break
  return content

/-- Lex a comment (after the %) -/
def lexComment : LexerM String := do
  takeUntil (· == '\n')

/-- Lex text content (non-special characters) -/
def lexText : LexerM String := do
  let isSpecial (c : Char) : Bool :=
    c == '\\' || c == '{' || c == '}' || c == '[' || c == ']' ||
    c == '$' || c == '%' || c == '\n' || c == ' ' || c == '\t'
  takeWhile (!isSpecial ·)

/-- Lex whitespace (spaces and tabs, not newlines) -/
def lexWhitespace : LexerM String := do
  takeWhile (fun c => c == ' ' || c == '\t')

/-- Lex a single token -/
def lexToken : LexerM Token := do
  let startPos ← getPosition

  match ← peek with
  | none =>
    return { kind := .eof, span := { start := startPos, stop := startPos }, raw := "" }

  | some '\\' =>
    let _ ← advance
    match ← peek with
    | some '[' =>
      let _ ← advance
      let content ← lexDisplayMathBracket
      let endPos ← getPosition
      let raw := "\\[" ++ content ++ "\\]"
      return { kind := .mathDisplay content, span := { start := startPos, stop := endPos }, raw }
    | some ']' =>
      let _ ← advance
      let endPos ← getPosition
      return { kind := .command "]", span := { start := startPos, stop := endPos }, raw := "\\]" }
    | _ =>
      let name ← lexCommandName
      let endPos ← getPosition
      return { kind := .command name, span := { start := startPos, stop := endPos }, raw := "\\" ++ name }

  | some '{' =>
    let _ ← advance
    let endPos ← getPosition
    return { kind := .braceOpen, span := { start := startPos, stop := endPos }, raw := "{" }

  | some '}' =>
    let _ ← advance
    let endPos ← getPosition
    return { kind := .braceClose, span := { start := startPos, stop := endPos }, raw := "}" }

  | some '[' =>
    let _ ← advance
    let endPos ← getPosition
    return { kind := .bracketOpen, span := { start := startPos, stop := endPos }, raw := "[" }

  | some ']' =>
    let _ ← advance
    let endPos ← getPosition
    return { kind := .bracketClose, span := { start := startPos, stop := endPos }, raw := "]" }

  | some '$' =>
    let _ ← advance
    match ← peek with
    | some '$' =>
      let _ ← advance
      let content ← lexDisplayMathDollar
      let endPos ← getPosition
      let raw := "$$" ++ content ++ "$$"
      return { kind := .mathDisplay content, span := { start := startPos, stop := endPos }, raw }
    | _ =>
      let content ← lexInlineMath
      let endPos ← getPosition
      let raw := "$" ++ content ++ "$"
      return { kind := .mathInline content, span := { start := startPos, stop := endPos }, raw }

  | some '%' =>
    let _ ← advance
    let content ← lexComment
    let endPos ← getPosition
    let raw := "%" ++ content
    return { kind := .comment content, span := { start := startPos, stop := endPos }, raw }

  | some '\n' =>
    let _ ← advance
    let ws ← lexWhitespace
    match ← peek with
    | some '\n' =>
      let _ ← advance
      while true do
        let _ ← lexWhitespace
        match ← peek with
        | some '\n' => let _ ← advance
        | _ => break
      let endPos ← getPosition
      return { kind := .paragraph, span := { start := startPos, stop := endPos }, raw := "\n" ++ ws ++ "\n" }
    | _ =>
      let endPos ← getPosition
      if ws.isEmpty then
        return { kind := .newline, span := { start := startPos, stop := endPos }, raw := "\n" }
      else
        return { kind := .whitespace ("\n" ++ ws), span := { start := startPos, stop := endPos }, raw := "\n" ++ ws }

  | some c =>
    if c == ' ' || c == '\t' then
      let ws ← lexWhitespace
      let endPos ← getPosition
      return { kind := .whitespace ws, span := { start := startPos, stop := endPos }, raw := ws }
    else
      let text ← lexText
      let endPos ← getPosition
      return { kind := .text text, span := { start := startPos, stop := endPos }, raw := text }

/-- Lex all tokens from the source -/
def lexAll : LexerM (Array Token) := do
  let mut tokens := #[]
  while true do
    let token ← lexToken
    tokens := tokens.push token
    if token.kind == .eof then
      break
  return tokens

end Lexer

/-- Tokenize a LaTeX source string -/
def tokenize (source : String) : Array Token :=
  let state : LexerState := { source, pos := 0, line := 1, column := 1 }
  (Lexer.lexAll.run state).1

end Runway.Latex
