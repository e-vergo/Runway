/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Runway.Latex.Ast

/-!
# LaTeX AST to HTML Conversion

Converts parsed LaTeX AST blocks to HTML for prose content rendering.
Math content is left as-is for MathJax processing in the browser.
-/

namespace Runway.Latex

/-- HTML builder state -/
structure HtmlState where
  /-- Accumulated HTML output -/
  html : String := ""
  /-- Module references found during conversion -/
  moduleRefs : Array Lean.Name := #[]
  /-- Node references found during conversion -/
  nodeRefs : Array String := #[]
  deriving Inhabited

/-- HTML builder monad -/
abbrev HtmlM := StateM HtmlState

namespace Html

/-- Append HTML string -/
def emit (s : String) : HtmlM Unit :=
  modify fun st => { st with html := st.html ++ s }

/-- Record a module reference -/
def addModuleRef (name : Lean.Name) : HtmlM Unit :=
  modify fun st => { st with moduleRefs := st.moduleRefs.push name }

/-- Record a node reference -/
def addNodeRef (label : String) : HtmlM Unit :=
  modify fun st => { st with nodeRefs := st.nodeRefs.push label }

/-- Escape HTML special characters -/
def escape (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
      | '<' => "&lt;"
      | '>' => "&gt;"
      | '&' => "&amp;"
      | '"' => "&quot;"
      | '\'' => "&#39;"
      | c => c.toString
  ) ""

/-- Convert inline content to HTML -/
partial def inlineToHtml : Inline → HtmlM Unit
  | .text s => emit (escape s)
  | .emph content => do
    emit "<em>"
    for c in content do inlineToHtml c
    emit "</em>"
  | .bold content => do
    emit "<strong>"
    for c in content do inlineToHtml c
    emit "</strong>"
  | .code s => do
    emit "<code>"
    emit (escape s)
    emit "</code>"
  | .math s => do
    -- Leave inline math for MathJax
    emit "\\("
    emit (escape s)
    emit "\\)"
  | .ref label => do
    -- Cross-reference - link to node
    emit s!"<a href=\"#{escape label}\" class=\"ref\">"
    emit (escape label)
    emit "</a>"
  | .cite keys => do
    -- Citation - render as reference list
    emit "<span class=\"cite\">["
    let keyList := keys.toList
    for h : i in [:keyList.length] do
      if i > 0 then emit ", "
      emit (escape keyList[i])
    emit "]</span>"
  | .href url text => do
    emit s!"<a href=\"{escape url}\">"
    for t in text do inlineToHtml t
    emit "</a>"
  | .lean names => do
    -- Link to Lean declarations
    emit "<span class=\"lean-refs\">"
    for h : i in [:names.size] do
      if i > 0 then emit ", "
      emit s!"<code class=\"lean-name\">{escape names[i].toString}</code>"
    emit "</span>"
  | .raw s => emit s  -- Raw LaTeX - pass through
  | .space => emit " "
  | .seq parts => do
    for p in parts do inlineToHtml p

/-- Convert inline array to HTML string -/
def inlinesToHtml (inlines : Array Inline) : HtmlM Unit := do
  for i in inlines do inlineToHtml i

/-- Convert a single Block to HTML -/
partial def blockToHtml : Block → HtmlM Unit
  | .document _ body => do
    for b in body do blockToHtml b
  | .chapter title label body => do
    emit "<section class=\"chapter\""
    match label with
    | some l => emit s!" id=\"{escape l}\""
    | none => pure ()
    emit ">\n"
    emit "<h1 class=\"chapter-title\">"
    inlineToHtml title
    emit "</h1>\n"
    for b in body do blockToHtml b
    emit "</section>\n"
  | .section level title label body => do
    let tag := s!"h{level + 1}"
    emit "<section class=\"section\""
    match label with
    | some l => emit s!" id=\"{escape l}\""
    | none => pure ()
    emit ">\n"
    emit s!"<{tag} class=\"section-title\">"
    inlineToHtml title
    emit s!"</{tag}>\n"
    for b in body do blockToHtml b
    emit "</section>\n"
  | .theorem env title md statement => do
    emit s!"<div class=\"theorem-env {escape env}\""
    match md.label with
    | some l => emit s!" id=\"{escape l}\""
    | none => pure ()
    emit ">\n"
    emit s!"<span class=\"theorem-type\">{escape env.capitalize}</span>"
    match title with
    | some t => do
      emit " <span class=\"theorem-title\">("
      inlineToHtml t
      emit ")</span>"
    | none => pure ()
    emit "\n<div class=\"theorem-statement\">\n"
    for b in statement do blockToHtml b
    emit "</div>\n</div>\n"
  | .proof md content => do
    emit "<div class=\"proof\""
    match md.label with
    | some l => emit s!" id=\"{escape l}\""
    | none => pure ()
    emit ">\n"
    emit "<span class=\"proof-title\">Proof.</span>\n"
    emit "<div class=\"proof-content\">\n"
    for b in content do blockToHtml b
    emit "</div>\n</div>\n"
  | .paragraph content => do
    emit "<p>"
    inlinesToHtml content
    emit "</p>\n"
  | .displayMath content => do
    -- Leave display math for MathJax
    emit "<div class=\"displaymath\">\\[\n"
    emit (escape content)
    emit "\n\\]</div>\n"
  | .itemize items => do
    emit "<ul>\n"
    for item in items do
      emit "<li>"
      for b in item do blockToHtml b
      emit "</li>\n"
    emit "</ul>\n"
  | .enumerate items => do
    emit "<ol>\n"
    for item in items do
      emit "<li>"
      for b in item do blockToHtml b
      emit "</li>\n"
    emit "</ol>\n"
  | .inputLeanModule moduleName => do
    -- Record module reference and emit a placeholder
    addModuleRef moduleName
    emit s!"<div class=\"lean-module-placeholder\" data-module=\"{escape moduleName.toString}\"></div>\n"
  | .inputLeanNode label => do
    -- Record node reference and emit a placeholder
    addNodeRef label
    emit s!"<div class=\"lean-node-placeholder\" data-node=\"{escape label}\"></div>\n"
  | .raw content => emit content
  | .comment _ => pure ()  -- Skip comments

/-- Convert blocks to HTML string -/
def blocksToHtml (blocks : Array Block) : HtmlM Unit := do
  for b in blocks do blockToHtml b

end Html

/-- Result of HTML conversion -/
structure HtmlResult where
  /-- The generated HTML string -/
  html : String
  /-- Module references found (for \inputleanmodule) -/
  moduleRefs : Array Lean.Name
  /-- Node references found (for \inputleannode) -/
  nodeRefs : Array String
  deriving Inhabited

/-- Convert LaTeX blocks to HTML -/
def toHtml (blocks : Array Block) : HtmlResult :=
  let (_, st) := Html.blocksToHtml blocks |>.run {}
  { html := st.html, moduleRefs := st.moduleRefs, nodeRefs := st.nodeRefs }

/-- Convert a document to HTML -/
def documentToHtml (doc : Document) : HtmlResult :=
  match doc.root with
  | .document _ body => toHtml body
  | other => toHtml #[other]

/-- Extract chapter info from a parsed document -/
structure ChapterExtract where
  /-- Chapter number (1-indexed) -/
  number : Nat
  /-- Chapter title as plain text -/
  title : String
  /-- Chapter label (if any) -/
  label : Option String
  /-- Whether this is an appendix chapter -/
  isAppendix : Bool
  /-- Chapter body blocks -/
  body : Array Block
  deriving Inhabited

/-- Extract chapters from a document body -/
def extractChapters (blocks : Array Block) : Array ChapterExtract := Id.run do
  let mut chapters : Array ChapterExtract := #[]
  let mut chapterNum := 1
  let mut inAppendix := false

  for block in blocks do
    match block with
    | .chapter title label body =>
      let titleText := title.toPlainText
      -- Check if this is the appendix marker
      if titleText.toLower == "appendix" then
        inAppendix := true
      chapters := chapters.push {
        number := chapterNum
        title := titleText
        label := label
        isAppendix := inAppendix
        body := body
      }
      chapterNum := chapterNum + 1
    | _ => pure ()

  return chapters

/-- Extract section info from chapter body -/
structure SectionExtract where
  /-- Section number within chapter -/
  number : Option Nat
  /-- Section title as plain text -/
  title : String
  /-- Section label (if any) -/
  label : Option String
  /-- Section body blocks -/
  body : Array Block
  deriving Inhabited

/-- Extract sections from a chapter body -/
def extractSections (body : Array Block) : Array SectionExtract := Id.run do
  let mut sections : Array SectionExtract := #[]
  let mut sectionNum := 1

  for block in body do
    match block with
    | .section level title label sectionBody =>
      if level == 1 then  -- Only top-level sections
        sections := sections.push {
          number := some sectionNum
          title := title.toPlainText
          label := label
          body := sectionBody
        }
        sectionNum := sectionNum + 1
    | _ => pure ()

  return sections

/-- Helper to flatten items arrays -/
private def flattenItems (items : Array (Array Block)) : Array Block :=
  items.foldl (fun acc item => acc ++ item) #[]

/-- Extract module references from blocks -/
partial def extractModuleRefs (blocks : Array Block) : Array Lean.Name := Id.run do
  let mut refs : Array Lean.Name := #[]
  let mut stack : Array Block := blocks.reverse

  while !stack.isEmpty do
    match stack.back? with
    | none => break
    | some block =>
      stack := stack.pop
      match block with
      | .inputLeanModule name => refs := refs.push name
      | .chapter _ _ body => stack := stack ++ body.reverse
      | .section _ _ _ body => stack := stack ++ body.reverse
      | .theorem _ _ _ statement => stack := stack ++ statement.reverse
      | .proof _ content => stack := stack ++ content.reverse
      | .itemize items => stack := stack ++ (flattenItems items).reverse
      | .enumerate items => stack := stack ++ (flattenItems items).reverse
      | .document _ body => stack := stack ++ body.reverse
      | _ => pure ()

  return refs

/-- Extract node references from blocks -/
partial def extractNodeRefs (blocks : Array Block) : Array String := Id.run do
  let mut refs : Array String := #[]
  let mut stack : Array Block := blocks.reverse

  while !stack.isEmpty do
    match stack.back? with
    | none => break
    | some block =>
      stack := stack.pop
      match block with
      | .inputLeanNode label => refs := refs.push label
      | .chapter _ _ body => stack := stack ++ body.reverse
      | .section _ _ _ body => stack := stack ++ body.reverse
      | .theorem _ _ _ statement => stack := stack ++ statement.reverse
      | .proof _ content => stack := stack ++ content.reverse
      | .itemize items => stack := stack ++ (flattenItems items).reverse
      | .enumerate items => stack := stack ++ (flattenItems items).reverse
      | .document _ body => stack := stack ++ body.reverse
      | _ => pure ()

  return refs

end Runway.Latex
