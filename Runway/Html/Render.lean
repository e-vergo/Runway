/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Verso.Output.Html
import Runway.Latex.Ast

/-!
# HTML Rendering

Converts LaTeX AST to HTML using Verso's Html type.
-/

open Verso.Output

namespace Runway.Html

/-- Render configuration -/
structure RenderConfig where
  /-- Base URL for assets -/
  baseUrl : String := ""
  /-- Include MathJax for math rendering -/
  includeMathJax : Bool := true
  /-- Custom CSS files to include -/
  cssFiles : Array String := #["blueprint.css"]
  /-- Custom JS files to include -/
  jsFiles : Array String := #["verso-code.js"]
  deriving Repr, Inhabited

/-- Render state for tracking labels and references -/
structure RenderState where
  /-- Map of labels to their HTML IDs -/
  labels : Std.HashMap String String := {}
  /-- Current chapter number -/
  chapterNum : Nat := 0
  /-- Current section numbers -/
  sectionNum : Array Nat := #[0, 0, 0]
  /-- Current theorem counters by type -/
  theoremNum : Std.HashMap String Nat := {}
  deriving Inhabited

/-- Render monad -/
abbrev RenderM := StateM RenderState

namespace Render

/-- Generate HTML ID from label -/
def labelToId (label : String) : String :=
  label.map fun c =>
    if c.isAlpha || c.isDigit || c == '-' || c == '_' then c
    else '-'

/-- Register a label and return its ID -/
def registerLabel (label : String) : RenderM String := do
  let id := labelToId label
  modify fun s => { s with labels := s.labels.insert label id }
  return id

/-- Get next theorem number for a given environment -/
def nextTheoremNum (env : String) : RenderM Nat := do
  let s ← get
  let n := s.theoremNum.get? env |>.getD 0
  set { s with theoremNum := s.theoremNum.insert env (n + 1) }
  return n + 1

/-- Intersperse an element between array elements -/
def intersperse (sep : α) (arr : Array α) : Array α :=
  if arr.isEmpty then #[]
  else
    arr.foldl (init := #[]) fun acc x =>
      if acc.isEmpty then #[x]
      else acc.push sep |>.push x

/-- Generate the MathJax configuration script -/
def mathJaxConfig : Html :=
  .tag "script" #[] (Html.text false
    "window.MathJax = {\n  tex: {\n    inlineMath: [['\\\\(', '\\\\)']],\n    displayMath: [['\\\\[', '\\\\]']],\n    processEscapes: true,\n    processEnvironments: true\n  },\n  options: {\n    skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']\n  }\n};")

/-- Generate the full HTML page -/
def renderPage (config : RenderConfig) (title : String) (body : Html) : Html :=
  let mathJax := if config.includeMathJax then
    mathJaxConfig ++
    .tag "script" #[("id", "MathJax-script"), ("async", ""),
        ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] Html.empty
  else Html.empty

  let cssLinks := config.cssFiles.map fun file =>
    Html.tag "link" #[("rel", "stylesheet"), ("href", config.baseUrl ++ file)] Html.empty

  let jsLinks := config.jsFiles.map fun file =>
    Html.tag "script" #[("src", config.baseUrl ++ file)] Html.empty

  .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1.0")] Html.empty ++
      .tag "title" #[] (Html.text true title) ++
      .seq cssLinks ++
      mathJax
    ) ++
    .tag "body" #[] (
      .tag "main" #[("class", "blueprint-content")] body ++
      .seq jsLinks
    )
  )

mutual

/-- Render array of inlines -/
partial def renderInlines (inlines : Array Latex.Inline) : Html :=
  .seq (inlines.map renderInline)

/-- Render inline content to HTML -/
partial def renderInline : Latex.Inline → Html
  | .text s => Html.text true s
  | .emph content => .tag "em" #[] (renderInlines content)
  | .bold content => .tag "strong" #[] (renderInlines content)
  | .code content => .tag "code" #[] (Html.text true content)
  | .math content => .tag "span" #[("class", "math inline")] (Html.text false ("\\(" ++ content ++ "\\)"))
  | .ref label => .tag "a" #[("href", "#" ++ labelToId label), ("class", "ref")] (Html.text true label)
  | .cite keys =>
    let links := keys.map fun key =>
      Html.tag "a" #[("href", "#bib-" ++ key), ("class", "citation")] (Html.text true key)
    .tag "span" #[("class", "cite")] (.seq (intersperse (Html.text true ", ") links))
  | .href url txt => .tag "a" #[("href", url)] (renderInlines txt)
  | .lean names =>
    let rendered := names.map fun n => Html.text true n.toString
    .tag "span" #[("class", "lean-names")] (.seq (intersperse (Html.text true ", ") rendered))
  | .raw content => Html.text false content
  | .space => Html.text true " "
  | .seq parts => renderInlines parts

/-- Render multiple blocks -/
partial def renderBlocks (config : RenderConfig) (blocks : Array Latex.Block) : RenderM Html := do
  let htmls ← blocks.mapM (renderBlock config)
  return .seq htmls

/-- Render a block to HTML -/
partial def renderBlock (config : RenderConfig) : Latex.Block → RenderM Html
  | .document preamble body => do
    let bodyHtml ← renderBlocks config body
    let title := preamble.title.getD "Blueprint"
    return renderPage config title bodyHtml

  | .chapter title label body => do
    let s ← get
    set { s with chapterNum := s.chapterNum + 1, sectionNum := #[0, 0, 0] }
    let num := s.chapterNum + 1
    let id ← match label with
      | some l => registerLabel l
      | none => pure s!"chapter-{num}"
    let titleHtml := renderInline title
    let bodyHtml ← renderBlocks config body
    return .tag "section" #[("id", id), ("class", "chapter")] (
      .tag "h1" #[] (.seq #[Html.text true s!"Chapter {num}. ", titleHtml]) ++
      bodyHtml
    )

  | .section level title label body => do
    let s ← get
    let nums := s.sectionNum.set! (level - 1) (s.sectionNum[level - 1]! + 1)
    set { s with sectionNum := nums }
    let id ← match label with
      | some l => registerLabel l
      | none => pure s!"section-{nums[0]!}-{nums[1]!}-{nums[2]!}"
    let titleHtml := renderInline title
    let bodyHtml ← renderBlocks config body
    let tagName := if level == 1 then "h2" else if level == 2 then "h3" else "h4"
    return .tag "section" #[("id", id), ("class", s!"section level-{level}")] (
      .tag tagName #[] titleHtml ++
      bodyHtml
    )

  | .theorem env _title metadata statement => do
    let num ← nextTheoremNum env
    let id ← match metadata.label with
      | some l => registerLabel l
      | none => pure s!"{env}-{num}"

    -- Determine status classes
    let statusClass := if metadata.leanOk then "proved"
      else if metadata.notReady then "not-ready"
      else "pending"

    let header := .tag "span" #[("class", "theorem-header")] (
      .tag "span" #[("class", "theorem-type")] (Html.text true (env.capitalize)) ++
      Html.text true " " ++
      .tag "span" #[("class", "theorem-number")] (Html.text true s!"{num}")
    )

    let statementHtml ← renderBlocks config statement

    -- Render the statement column
    let statementCol := .tag "div" #[("class", "sbs-statement-column")] (
      header ++
      .tag "div" #[("class", "theorem-statement")] statementHtml
    )

    -- Render the Lean column (placeholder for Dress injection)
    let leanCol := match metadata.signatureHtml with
      | some html => .tag "div" #[("class", "sbs-lean-column")]
          (.tag "pre" #[("class", "lean-code")] (Html.text false html))
      | none => .tag "div" #[("class", "sbs-lean-column placeholder")]
          (Html.text true "Lean code will appear here")

    return .tag "div" #[("id", id), ("class", s!"theorem-block {env} {statusClass}")] (
      .tag "div" #[("class", "sbs-container")] (statementCol ++ leanCol)
    )

  | .proof metadata content => do
    let id ← match metadata.label with
      | some l => registerLabel l
      | none => pure "proof"

    let contentHtml ← renderBlocks config content

    -- Render statement column
    let statementCol := .tag "div" #[("class", "sbs-statement-column")] (
      .tag "details" #[("class", "proof")] (
        .tag "summary" #[] (Html.text true "Proof") ++
        contentHtml
      )
    )

    -- Render Lean proof column
    let leanCol := match metadata.proofHtml with
      | some html => .tag "div" #[("class", "sbs-lean-column")]
          (.tag "pre" #[("class", "lean-code")] (Html.text false html))
      | none => Html.empty

    return .tag "div" #[("id", id), ("class", "proof-block sbs-container")] (
      statementCol ++ leanCol
    )

  | .paragraph content =>
    return .tag "p" #[] (renderInlines content)

  | .displayMath content =>
    return .tag "div" #[("class", "math display")] (
      Html.text false ("\\[" ++ content ++ "\\]")
    )

  | .itemize items => do
    let itemsHtml ← items.mapM fun item => do
      let content ← renderBlocks config item
      return .tag "li" #[] content
    return .tag "ul" #[] (.seq itemsHtml)

  | .enumerate items => do
    let itemsHtml ← items.mapM fun item => do
      let content ← renderBlocks config item
      return .tag "li" #[] content
    return .tag "ol" #[] (.seq itemsHtml)

  | .inputLeanModule moduleName =>
    return .tag "div" #[("class", "lean-module"), ("data-module", moduleName.toString)]
      (Html.text true s!"[Lean module: {moduleName}]")

  | .inputLeanNode label => do
    let _ ← registerLabel label
    return .tag "div" #[("id", labelToId label), ("class", "lean-node"), ("data-label", label)]
      (Html.text true s!"[Lean node: {label}]")

  | .paperStatement label => do
    let _ ← registerLabel label
    return .tag "div" #[("id", labelToId label), ("class", "paper-statement"), ("data-label", label)]
      (Html.text true s!"[Paper statement: {label}]")

  | .paperFull label => do
    let _ ← registerLabel label
    return .tag "div" #[("id", labelToId label), ("class", "paper-full"), ("data-label", label)]
      (Html.text true s!"[Paper full: {label}]")

  | .paperProof label => do
    let _ ← registerLabel label
    return .tag "div" #[("id", labelToId label), ("class", "paper-proof"), ("data-label", label)]
      (Html.text true s!"[Paper proof: {label}]")

  | .raw content =>
    return Html.text false content

  | .comment _ =>
    return Html.empty

end

end Render

/-- Render a LaTeX document to HTML string -/
def render (doc : Latex.Document) (config : RenderConfig := {}) : String :=
  let initialState : RenderState := {}
  let (html, _) := (Render.renderBlock config doc.root).run initialState
  Html.doctype ++ "\n" ++ html.asString

/-- Render a LaTeX document to HTML and write to file -/
def renderToFile (doc : Latex.Document) (path : System.FilePath)
    (config : RenderConfig := {}) : IO Unit := do
  let content := render doc config
  IO.FS.writeFile path content

end Runway.Html
