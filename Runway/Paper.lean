/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Runway.Site
import Runway.Config
import Runway.Latex
import Runway.Graph
import Runway.Render
import Runway.Macros
import Verso.Output.Html
import Std.Data.HashMap
import Dress.Render

/-!
# ar5iv-Style Paper Generation

This module provides types and rendering functions for generating
academic-style HTML papers that pull content from @[blueprint] attributes.
-/

namespace Runway.Paper

open Verso.Output Html
open Std (HashMap)
open Runway.Graph (NodeStatus)
open Runway (divClass)

/-- Verification status for badge display -/
inductive VerificationLevel
  | notStarted   -- No Lean code attached
  | inProgress   -- Has sorry or incomplete
  | verified     -- Fully proven
  deriving Inhabited, Repr

/-- Determine verification level from node status string -/
def toVerificationLevel (status : String) : VerificationLevel :=
  if status == "proven" || status == "fullyProven" || status == "mathlibReady" || status == "inMathlib" then
    .verified
  else if status == "sorry" || status == "stated" || status == "ready" then
    .inProgress
  else
    .notStarted

/-- Convert VerificationLevel back to Dress.Graph.NodeStatus for rendering.
    Note: This is a lossy conversion since VerificationLevel has fewer states. -/
def verificationLevelToDressStatus : VerificationLevel → Dress.Graph.NodeStatus
  | .verified => .fullyProven
  | .inProgress => .stated
  | .notStarted => .notReady

/-- Render verification badge HTML -/
def renderBadge (level : VerificationLevel) : Html :=
  let (cls, icon, label) := match level with
    | .verified => ("verified", "check", "Verified")
    | .inProgress => ("in-progress", "half", "In Progress")
    | .notStarted => ("not-started", "circle", "Not Started")
  .tag "span" #[("class", s!"verification-badge {cls}"),
               ("title", "Formalization status")] (
    .tag "span" #[("class", s!"badge-icon badge-icon-{icon}")] Html.empty ++
    .tag "span" #[("class", "badge-text")] (Html.text true label)
  )

/-- Info needed to render a paper theorem -/
structure PaperNodeInfo where
  label : String
  envType : String          -- "theorem", "lemma", "definition"
  displayNumber : String    -- "4.1.2" from blueprint numbering
  statement : String        -- LaTeX statement text
  proof : Option String     -- LaTeX proof text (if available)
  status : VerificationLevel
  blueprintUrl : Option String  -- Link back to blueprint page
  deriving Inhabited

/-- Capitalize first letter of a string -/
private def capitalize (s : String) : String :=
  match s.toList with
  | [] => s
  | c :: cs => String.ofList (c.toUpper :: cs)

/-- Extended paper node info with Lean code fields -/
structure PaperNodeInfoExt extends PaperNodeInfo where
  /-- Pre-rendered Lean signature HTML (syntax-highlighted) -/
  signatureHtml : Option String := none
  /-- Pre-rendered Lean proof body HTML (syntax-highlighted) -/
  proofBodyHtml : Option String := none
  /-- Hover data JSON for Tippy.js tooltips -/
  hoverData : Option String := none
  deriving Inhabited

/-- Convert PaperNodeInfoExt to SbsData for consolidated rendering -/
def PaperNodeInfoExt.toSbsData (info : PaperNodeInfoExt) (includeProof : Bool := true) : Dress.Render.SbsData := {
  id := info.label
  label := info.displayNumber
  displayNumber := some info.displayNumber
  envType := info.envType
  status := verificationLevelToDressStatus info.status
  statementHtml := info.statement
  proofHtml := if includeProof then info.proof else none
  signatureHtml := info.signatureHtml
  proofBodyHtml := if includeProof then info.proofBodyHtml else none
  hoverData := info.hoverData
  declNames := #[]  -- Paper doesn't track this
}

/-- Render the Lean column for side-by-side display
    DEPRECATED: Use Dress.Render.renderSideBySide with .paper variant instead -/
def renderLeanColumn (info : PaperNodeInfoExt) : Html :=
  Html.text false (Dress.Render.renderLeanColumn info.toSbsData)

/-- Render theorem environment (statement only) -/
def renderStatement (info : PaperNodeInfo) : Html :=
  .tag "div" #[("class", s!"paper-theorem paper-{info.envType}"),
               ("id", info.label)] (
    .tag "div" #[("class", "paper-theorem-header")] (
      .tag "span" #[("class", "paper-theorem-type")] (
        Html.text true (capitalize info.envType ++ " " ++ info.displayNumber)
      ) ++
      renderBadge info.status ++
      (match info.blueprintUrl with
       | some url => .tag "a" #[("class", "blueprint-link"), ("href", url)]
                       (Html.text true "[blueprint]")
       | none => Html.empty)
    ) ++
    .tag "div" #[("class", "paper-theorem-statement")] (
      Html.text false info.statement  -- Raw LaTeX for MathJax
    )
  )

/-- Render statement with side-by-side Lean code (no proof) -/
def renderStatementSbs (info : PaperNodeInfoExt) : Html :=
  let sbsData := info.toSbsData (includeProof := false)
  Html.text false (Dress.Render.renderSideBySide sbsData (.paper info.blueprintUrl))

/-- Render theorem with proof -/
def renderFull (info : PaperNodeInfo) : Html :=
  renderStatement info ++
  (match info.proof with
   | some proofText =>
     .tag "div" #[("class", "paper-proof")] (
       .tag "span" #[("class", "paper-proof-header")] (Html.text true "Proof. ") ++
       .tag "span" #[("class", "paper-proof-body")] (Html.text false proofText)
     )
   | none => Html.empty)

/-- Render theorem with proof in side-by-side layout -/
def renderFullSbs (info : PaperNodeInfoExt) : Html :=
  let sbsData := info.toSbsData (includeProof := true)
  Html.text false (Dress.Render.renderSideBySide sbsData (.paper info.blueprintUrl))

/-- Render proof only (for deferred proofs) -/
def renderProofOnly (info : PaperNodeInfo) : Html :=
  match info.proof with
  | some proofText =>
    .tag "div" #[("class", "paper-proof"), ("id", s!"{info.label}-proof")] (
      .tag "span" #[("class", "paper-proof-header")] (
        Html.text true s!"Proof of {capitalize info.envType} {info.displayNumber}. "
      ) ++
      .tag "span" #[("class", "paper-proof-body")] (Html.text false proofText)
    )
  | none => Html.empty

/-- Generate paper header (title, authors, abstract) -/
def renderHeader (config : Config) : Html :=
  .tag "header" #[("class", "paper-header")] (
    .tag "h1" #[("class", "paper-title")] (
      Html.text true (config.paperTitle.getD config.title)
    ) ++
    (if config.paperAuthors.isEmpty then Html.empty else
      .tag "div" #[("class", "paper-authors")] (
        Html.text true (", ".intercalate config.paperAuthors.toList)
      )
    ) ++
    (match config.paperAbstract with
     | some abstract =>
       .tag "div" #[("class", "paper-abstract")] (
         .tag "strong" #[] (Html.text true "Abstract. ") ++
         Html.text false abstract
       )
     | none => Html.empty)
  )

/-- Render paper content (header, body, footer) wrapped in ar5iv-paper div.
    This is the content that goes inside the sidebar template. -/
def renderPaperContent (config : Config) (content : Html) : Html :=
  divClass "ar5iv-paper" (
    renderHeader config ++
    .tag "main" #[("class", "paper-content")] content ++
    .tag "footer" #[("class", "paper-footer")] (
      Html.text true "Generated with " ++
      .tag "a" #[("href", "https://github.com/e-vergo/Side-By-Side-Blueprint")]
        (Html.text true "Side-by-Side Blueprint")
    )
  )

/-- Full paper HTML page (standalone, deprecated - use renderPaperContent with sidebar template) -/
def renderPaperPage (config : Config) (content : Html) : Html :=
  let mathjaxConfig := .tag "script" #[] (Html.text false (Macros.generateMathJaxConfig config.mathjaxMacrosJson))
  .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true (config.paperTitle.getD config.title)) ++
      .tag "link" #[("rel", "stylesheet"), ("href", "assets/paper.css")] Html.empty ++
      mathjaxConfig ++
      .tag "script" #[("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
                      ("async", "true")] Html.empty
    ) ++
    .tag "body" #[("class", "ar5iv-paper")] (
      renderHeader config ++
      .tag "main" #[("class", "paper-content")] content ++
      .tag "footer" #[("class", "paper-footer")] (
        Html.text true "Generated with " ++
        .tag "a" #[("href", "https://github.com/e-vergo/Side-By-Side-Blueprint")]
          (Html.text true "Side-by-Side Blueprint")
      )
    )
  )

/-! ## Document Conversion -/

/-- Tag types for tracking which HTML tags are open -/
private inductive TagType
  | code   -- \texttt{...}
  | em     -- \emph{...}
  | strong -- \textbf{...}
  deriving BEq

private def TagType.closeTag : TagType → String
  | .code => "</code>"
  | .em => "</em>"
  | .strong => "</strong>"

/-- Convert LaTeX text commands to HTML (for content outside math mode).
    Handles: \texttt{...} -> <code>...</code>
             \emph{...} -> <em>...</em>
             \textbf{...} -> <strong>...</strong>
    Note: Content inside $...$ or \[...\] is left untouched for MathJax. -/
partial def latexTextToHtml (s : String) : String :=
  go s.toList "" false []
where
  go (input : List Char) (acc : String) (inMath : Bool) (tagStack : List TagType) : String :=
    match input with
    | [] => acc
    | '\\' :: 't' :: 'e' :: 'x' :: 't' :: 't' :: 't' :: '{' :: rest =>
      if inMath then
        -- Inside math mode, keep as-is
        go rest (acc ++ "\\texttt{") inMath tagStack
      else
        -- Convert \texttt{ to <code>
        go rest (acc ++ "<code>") inMath (.code :: tagStack)
    | '\\' :: 't' :: 'e' :: 'x' :: 't' :: 'b' :: 'f' :: '{' :: rest =>
      if inMath then
        -- Inside math mode, keep as-is
        go rest (acc ++ "\\textbf{") inMath tagStack
      else
        -- Convert \textbf{ to <strong>
        go rest (acc ++ "<strong>") inMath (.strong :: tagStack)
    | '\\' :: 'e' :: 'm' :: 'p' :: 'h' :: '{' :: rest =>
      if inMath then
        -- Inside math mode, keep as-is
        go rest (acc ++ "\\emph{") inMath tagStack
      else
        -- Convert \emph{ to <em>
        go rest (acc ++ "<em>") inMath (.em :: tagStack)
    | '$' :: '$' :: rest =>
      -- Display math $$ toggles
      go rest (acc ++ "$$") (!inMath) tagStack
    | '$' :: rest =>
      -- Inline math $ toggles
      go rest (acc ++ "$") (!inMath) tagStack
    | '\\' :: '[' :: rest =>
      -- Display math \[ starts
      go rest (acc ++ "\\[") true tagStack
    | '\\' :: ']' :: rest =>
      -- Display math \] ends
      go rest (acc ++ "\\]") false tagStack
    | '\\' :: '(' :: rest =>
      -- Inline math \( starts
      go rest (acc ++ "\\(") true tagStack
    | '\\' :: ')' :: rest =>
      -- Inline math \) ends
      go rest (acc ++ "\\)") false tagStack
    | '}' :: rest =>
      if inMath then
        go rest (acc ++ "}") inMath tagStack
      else
        match tagStack with
        | openTag :: remaining => go rest (acc ++ openTag.closeTag) inMath remaining
        | [] => go rest (acc ++ "}") inMath tagStack
    | c :: rest =>
      go rest (acc.push c) inMath tagStack

/-- Convert NodeStatus to VerificationLevel -/
def nodeStatusToVerificationLevel (status : NodeStatus) : VerificationLevel :=
  match status with
  | .proven | .fullyProven | .mathlibReady | .inMathlib => .verified
  | .sorry | .stated | .ready => .inProgress
  | .notReady => .notStarted

/-- Build a PaperNodeInfo from a NodeInfo -/
def toPaperNodeInfo (node : NodeInfo) (baseUrl : String := "") : PaperNodeInfo :=
  { label := node.label
    envType := node.envType
    displayNumber := node.displayNumber.getD node.label
    statement := latexTextToHtml node.statementHtml
    proof := node.proofHtml.map latexTextToHtml
    status := nodeStatusToVerificationLevel node.status
    blueprintUrl := some (baseUrl ++ "index.html#node-" ++ node.label)
  }

/-- Build a PaperNodeInfoExt from a NodeInfo (includes Lean code fields) -/
def toPaperNodeInfoExt (node : NodeInfo) (baseUrl : String := "") : PaperNodeInfoExt :=
  { label := node.label
    envType := node.envType
    displayNumber := node.displayNumber.getD node.label
    statement := latexTextToHtml node.statementHtml
    proof := node.proofHtml.map latexTextToHtml
    status := nodeStatusToVerificationLevel node.status
    blueprintUrl := some (baseUrl ++ "index.html#" ++ node.label)
    signatureHtml := node.signatureHtml
    proofBodyHtml := node.proofBodyHtml
    hoverData := node.hoverData
  }

/-- Render an inline element to HTML string -/
private partial def inlineToHtml : Latex.Inline -> String
  | .text s => escapeHtml s
  | .emph content => s!"<em>{content.foldl (fun acc c => acc ++ inlineToHtml c) ""}</em>"
  | .bold content => s!"<strong>{content.foldl (fun acc c => acc ++ inlineToHtml c) ""}</strong>"
  | .code s => s!"<code>{escapeHtml s}</code>"
  | .math s => s!"\\({escapeHtml s}\\)"
  | .ref label => s!"<a href=\"#{escapeHtml label}\" class=\"ref\">{escapeHtml label}</a>"
  | .cite keys => s!"<span class=\"cite\">[{",".intercalate keys.toList}]</span>"
  | .href url linkText => s!"<a href=\"{escapeHtml url}\">{linkText.foldl (fun acc c => acc ++ inlineToHtml c) ""}</a>"
  | .lean names => s!"<span class=\"lean-refs\">{names.map (·.toString) |>.toList |> ", ".intercalate}</span>"
  | .raw s => s
  | .space => " "
  | .seq parts => parts.foldl (fun acc p => acc ++ inlineToHtml p) ""
where
  escapeHtml (s : String) : String :=
    s.foldl (fun acc c => acc ++ match c with
      | '<' => "&lt;"
      | '>' => "&gt;"
      | '&' => "&amp;"
      | '"' => "&quot;"
      | '\'' => "&#39;"
      | c => c.toString) ""

/-- Render a block to HTML string (for prose content) -/
private partial def blockToHtmlString (artifacts : HashMap String NodeInfo) (baseUrl : String) : Latex.Block -> String
  | .document _ body => body.foldl (fun acc b => acc ++ blockToHtmlString artifacts baseUrl b) ""
  | .chapter title label body =>
    let labelAttr := match label with
      | some l => s!" id=\"{l}\""
      | none => ""
    let titleHtml := inlineToHtml title
    s!"<section class=\"paper-chapter\"{labelAttr}>\n<h1>{titleHtml}</h1>\n{body.foldl (fun acc b => acc ++ blockToHtmlString artifacts baseUrl b) ""}</section>\n"
  | .section level title label body =>
    let labelAttr := match label with
      | some l => s!" id=\"{l}\""
      | none => ""
    let titleHtml := inlineToHtml title
    let tag := s!"h{level + 1}"
    s!"<section class=\"paper-section\"{labelAttr}>\n<{tag}>{titleHtml}</{tag}>\n{body.foldl (fun acc b => acc ++ blockToHtmlString artifacts baseUrl b) ""}</section>\n"
  | .theorem env title md statement =>
    let labelAttr := match md.label with
      | some l => s!" id=\"{l}\""
      | none => ""
    let titleHtml := match title with
      | some t => s!" <span class=\"theorem-title\">({inlineToHtml t})</span>"
      | none => ""
    let stmtHtml := statement.foldl (fun acc b => acc ++ blockToHtmlString artifacts baseUrl b) ""
    s!"<div class=\"paper-theorem-env {env}\"{labelAttr}>\n<span class=\"theorem-type\">{env.capitalize}</span>{titleHtml}\n<div class=\"theorem-statement\">\n{stmtHtml}</div>\n</div>\n"
  | .proof md content =>
    let labelAttr := match md.label with
      | some l => s!" id=\"{l}\""
      | none => ""
    let contentHtml := content.foldl (fun acc b => acc ++ blockToHtmlString artifacts baseUrl b) ""
    s!"<div class=\"paper-proof\"{labelAttr}>\n<span class=\"proof-title\">Proof.</span>\n<div class=\"proof-content\">\n{contentHtml}</div>\n</div>\n"
  | .paragraph content =>
    s!"<p>{content.foldl (fun acc i => acc ++ inlineToHtml i) ""}</p>\n"
  | .displayMath content =>
    s!"<div class=\"displaymath\">\\[\n{content}\n\\]</div>\n"
  | .itemize items =>
    let itemsHtml := items.foldl (fun acc item =>
      acc ++ "<li>" ++ item.foldl (fun a b => a ++ blockToHtmlString artifacts baseUrl b) "" ++ "</li>\n") ""
    s!"<ul>\n{itemsHtml}</ul>\n"
  | .enumerate items =>
    let itemsHtml := items.foldl (fun acc item =>
      acc ++ "<li>" ++ item.foldl (fun a b => a ++ blockToHtmlString artifacts baseUrl b) "" ++ "</li>\n") ""
    s!"<ol>\n{itemsHtml}</ol>\n"
  | .inputLeanModule _ => ""  -- Not used in paper mode
  | .inputLeanNode label =>
    -- Resolve the node and render full side-by-side (statement + proof + Lean code)
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node =>
      let paperNode := toPaperNodeInfoExt node baseUrl
      (renderFullSbs paperNode).asString
    | none => s!"<div class=\"paper-error\">Node not found: {label}</div>\n"
  | .paperStatement label =>
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node =>
      let paperNode := toPaperNodeInfoExt node baseUrl
      (renderStatementSbs paperNode).asString
    | none => s!"<div class=\"paper-error\">Node not found: {label}</div>\n"
  | .paperFull label =>
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node =>
      let paperNode := toPaperNodeInfoExt node baseUrl
      (renderFullSbs paperNode).asString
    | none => s!"<div class=\"paper-error\">Node not found: {label}</div>\n"
  | .paperProof label =>
    -- For proof-only, use the non-SBS version (just LaTeX proof)
    let normalizedLabel := label.replace ":" "-"
    match artifacts.get? normalizedLabel with
    | some node =>
      let paperNode := toPaperNodeInfo node baseUrl
      (renderProofOnly paperNode).asString
    | none => s!"<div class=\"paper-error\">Proof not found: {label}</div>\n"
  | .raw content => content
  | .comment _ => ""

/-- Convert a parsed document to paper HTML, resolving paper hooks -/
def convertDocument (doc : Latex.Document)
    (artifacts : HashMap String NodeInfo)
    (config : Config) : Html :=
  let baseUrl := config.baseUrl
  let contentHtml := match doc.root with
    | .document _ body => body.foldl (fun acc b => acc ++ blockToHtmlString artifacts baseUrl b) ""
    | other => blockToHtmlString artifacts baseUrl other
  Html.text false contentHtml

end Runway.Paper
