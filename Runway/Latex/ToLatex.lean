/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Runway.Latex.Ast
import Runway.Site
import Std.Data.HashMap

/-!
# LaTeX AST to LaTeX Conversion

Converts parsed LaTeX AST back to LaTeX string, resolving node references
against Dress artifacts. Used for PDF generation where `\inputleannode{label}`
hooks are expanded to actual theorem content.
-/

namespace Runway.Latex

open Std (HashMap)

/-- Configuration for LaTeX output -/
structure LatexConfig where
  /-- Whether to preserve and emit the source document's preamble -/
  preserveSourcePreamble : Bool := true
  deriving Inhabited, Repr

/-- LaTeX builder state -/
structure LatexState where
  /-- Accumulated LaTeX output -/
  latex : String := ""
  /-- Node labels referenced via \inputleannode etc. -/
  nodeRefs : Array String := #[]
  /-- Warnings generated during conversion -/
  warnings : Array String := #[]
  deriving Inhabited

/-- LaTeX builder monad -/
abbrev LatexM := StateM LatexState

namespace Latex

/-- Append LaTeX string -/
def emit (s : String) : LatexM Unit :=
  modify fun st => { st with latex := st.latex ++ s }

/-- Append LaTeX string with newline -/
def emitLn (s : String) : LatexM Unit :=
  emit (s ++ "\n")

/-- Record a node reference -/
def addNodeRef (label : String) : LatexM Unit :=
  modify fun st => { st with nodeRefs := st.nodeRefs.push label }

/-- Record a warning -/
def addWarning (msg : String) : LatexM Unit :=
  modify fun st => { st with warnings := st.warnings.push msg }

/-- Capitalize first letter of a string -/
private def capitalize (s : String) : String :=
  match s.toList with
  | [] => s
  | c :: cs => String.ofList (c.toUpper :: cs)

/-- Wrap a string in braces: s -> {s} -/
private def braces (s : String) : String := "{" ++ s ++ "}"

/-- Convert inline content to LaTeX -/
partial def inlineToLatex : Inline → LatexM Unit
  | .text s => emit s  -- Text passes through as-is (already LaTeX)
  | .emph content => do
    emit "\\emph{"
    for c in content do inlineToLatex c
    emit "}"
  | .bold content => do
    emit "\\textbf{"
    for c in content do inlineToLatex c
    emit "}"
  | .code s => emit ("\\texttt{" ++ s ++ "}")
  | .math s => emit s!"${s}$"  -- Inline math passes through
  | .ref label => emit ("\\ref" ++ braces label)
  | .cite keys => do
    let keyStr := ",".intercalate keys.toList
    emit ("\\cite" ++ braces keyStr)
  | .href url text => do
    emit "\\href{"
    emit url
    emit "}{"
    for t in text do inlineToLatex t
    emit "}"
  | .lean names => do
    -- Render Lean names as code
    emit "\\texttt{"
    let nameStrs := names.map (·.toString)
    emit (", ".intercalate nameStrs.toList)
    emit "}"
  | .raw s => emit s  -- Raw LaTeX passes through
  | .space => emit " "
  | .seq parts => do
    for p in parts do inlineToLatex p

/-- Convert inline array to LaTeX -/
def inlinesToLatex (inlines : Array Inline) : LatexM Unit := do
  for i in inlines do inlineToLatex i

/-- Convert a single Block to LaTeX, resolving node references against artifacts -/
partial def blockToLatex (artifacts : HashMap String NodeInfo) : Block → LatexM Unit
  | .document _ body => do
    for b in body do blockToLatex artifacts b
  | .chapter title label body => do
    emit "\\chapter{"
    inlineToLatex title
    emit "}\n"
    if let some l := label then emitLn ("\\label" ++ braces l)
    emitLn ""
    for b in body do blockToLatex artifacts b
  | .section level title label body => do
    let cmd := match level with
      | 0 => "section"
      | 1 => "subsection"
      | _ => "subsubsection"
    emit ("\\" ++ cmd ++ "{")
    inlineToLatex title
    emit "}\n"
    if let some l := label then emitLn ("\\label" ++ braces l)
    emitLn ""
    for b in body do blockToLatex artifacts b
  | .theorem env title md statement => do
    emit ("\\begin" ++ braces env)
    if let some t := title then
      emit "["
      inlineToLatex t
      emit "]"
    emit "\n"
    if let some l := md.label then emitLn ("\\label" ++ braces l)
    for b in statement do blockToLatex artifacts b
    emitLn ("\\end" ++ braces env)
    emitLn ""
  | .proof md content => do
    emitLn "\\begin{proof}"
    if let some l := md.label then emitLn ("\\label" ++ braces l)
    for b in content do blockToLatex artifacts b
    emitLn "\\end{proof}"
    emitLn ""
  | .paragraph content => do
    inlinesToLatex content
    emitLn ""
    emitLn ""
  | .displayMath content => do
    emitLn "\\["
    emitLn content
    emitLn "\\]"
    emitLn ""
  | .itemize items => do
    emitLn "\\begin{itemize}"
    for item in items do
      emit "\\item "
      for b in item do blockToLatex artifacts b
    emitLn "\\end{itemize}"
    emitLn ""
  | .enumerate items => do
    emitLn "\\begin{enumerate}"
    for item in items do
      emit "\\item "
      for b in item do blockToLatex artifacts b
    emitLn "\\end{enumerate}"
    emitLn ""
  | .inputLeanModule _ => pure ()  -- Skip module imports in PDF output
  | .inputLeanNode label => do
    addNodeRef label
    -- Resolve node and emit theorem + proof
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node => do
      -- Emit theorem environment
      emitLn ("\\begin" ++ braces node.envType)
      emitLn ("\\label" ++ braces label)
      -- statementHtml contains LaTeX content (despite the name)
      emit node.statementHtml
      emit "\n"
      emitLn ("\\end" ++ braces node.envType)
      -- Emit proof if present
      if let some proofContent := node.proofHtml then
        emitLn "\\begin{proof}"
        emit proofContent
        emit "\n"
        emitLn "\\end{proof}"
      emitLn ""
    | none => do
      addWarning ("Node not found: " ++ label)
      emitLn ("% WARNING: Node not found: " ++ label)
  | .paperStatement label => do
    addNodeRef label
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node => do
      emitLn ("\\begin" ++ braces node.envType)
      emitLn ("\\label" ++ braces label)
      emit node.statementHtml
      emit "\n"
      emitLn ("\\end" ++ braces node.envType)
      emitLn ""
    | none => do
      addWarning ("Node not found: " ++ label)
      emitLn ("% WARNING: Node not found: " ++ label)
  | .paperFull label => do
    addNodeRef label
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node => do
      emitLn ("\\begin" ++ braces node.envType)
      emitLn ("\\label" ++ braces label)
      emit node.statementHtml
      emit "\n"
      emitLn ("\\end" ++ braces node.envType)
      if let some proofContent := node.proofHtml then
        emitLn "\\begin{proof}"
        emit proofContent
        emit "\n"
        emitLn "\\end{proof}"
      emitLn ""
    | none => do
      addWarning ("Node not found: " ++ label)
      emitLn ("% WARNING: Node not found: " ++ label)
  | .paperProof label => do
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node =>
      if let some proofContent := node.proofHtml then
        emitLn "\\begin{proof}"
        emit proofContent
        emit "\n"
        emitLn "\\end{proof}"
        emitLn ""
    | none => do
      addWarning ("Proof not found: " ++ label)
      emitLn ("% WARNING: Proof not found: " ++ label)
  | .raw content => emit content
  | .comment _ => pure ()  -- Skip comments

/-- Convert blocks to LaTeX -/
def blocksToLatex (artifacts : HashMap String NodeInfo) (blocks : Array Block) : LatexM Unit := do
  for b in blocks do blockToLatex artifacts b

/-- Generate preamble LaTeX from Preamble structure -/
def preambleToLatex (p : Preamble) : String := Id.run do
  let mut out := ""

  -- Document class
  if p.classOptions.isEmpty then
    out := out ++ "\\documentclass" ++ braces p.documentClass ++ "\n"
  else
    let optStr := ",".intercalate p.classOptions.toList
    out := out ++ "\\documentclass[" ++ optStr ++ "]" ++ braces p.documentClass ++ "\n"

  out := out ++ "\n"

  -- Packages
  for (pkg, opts) in p.packages do
    if opts.isEmpty then
      out := out ++ "\\usepackage" ++ braces pkg ++ "\n"
    else
      let optStr := ",".intercalate opts.toList
      out := out ++ "\\usepackage[" ++ optStr ++ "]" ++ braces pkg ++ "\n"

  out := out ++ "\n"

  -- Title, author, date
  if let some title := p.title then
    out := out ++ "\\title" ++ braces title ++ "\n"
  if let some author := p.author then
    out := out ++ "\\author" ++ braces author ++ "\n"
  if let some date := p.date then
    out := out ++ "\\date" ++ braces date ++ "\n"

  -- Raw preamble content (custom macros, theorem definitions, etc.)
  if !p.rawContent.isEmpty then
    out := out ++ "\n"
    out := out ++ p.rawContent
    out := out ++ "\n"

  return out

end Latex

/-- Result of LaTeX conversion -/
structure LatexResult where
  /-- The generated LaTeX string -/
  latex : String
  /-- Node labels referenced -/
  nodeRefs : Array String
  /-- Warnings generated during conversion -/
  warnings : Array String
  deriving Inhabited

/-- Convert a document to LaTeX with node resolution -/
def documentToLatex (doc : Document) (artifacts : HashMap String NodeInfo)
    (config : LatexConfig := {}) : LatexResult :=
  let action : LatexM Unit := do
    match doc.root with
    | .document preamble body =>
      -- Emit preamble if configured
      if config.preserveSourcePreamble then
        Latex.emit (Latex.preambleToLatex preamble)
        Latex.emitLn ""
        Latex.emitLn "\\begin{document}"
        Latex.emitLn ""
        -- Emit maketitle if title was set
        if preamble.title.isSome then
          Latex.emitLn "\\maketitle"
          Latex.emitLn ""
      -- Convert body
      Latex.blocksToLatex artifacts body
      -- Close document if we emitted preamble
      if config.preserveSourcePreamble then
        Latex.emitLn ""
        Latex.emitLn "\\end{document}"
    | other =>
      -- Non-document block - just convert it
      Latex.blockToLatex artifacts other
  let (_, st) := action.run {}
  { latex := st.latex, nodeRefs := st.nodeRefs, warnings := st.warnings }

end Runway.Latex
