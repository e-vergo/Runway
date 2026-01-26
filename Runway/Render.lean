/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Verso.Doc
import Verso.Output.Html
import Runway.Config
import Runway.Genre
import Runway.Graph
import Runway.Site
import Runway.Traverse
import Runway.DepGraph
import Runway.DocGen4

/-!
# HTML Rendering for Blueprint

Implements HTML rendering using Verso's HtmlT monad for Blueprint documents.
This provides context-aware rendering of nodes, pages, and the overall site.

## Architecture

The rendering system follows VersoBlog patterns:
- `RenderM` monad provides access to Blueprint context and state
- Node rendering produces side-by-side layout (statement + Lean code)
- Page rendering composes multiple nodes
- Site rendering produces the complete HTML output
-/

namespace Runway

open Lean (Name)
open Verso Doc Output Html
open Runway.Graph (NodeStatus Graph Node Edge)

/-! ## Render Monad -/

/-- Read-only context for rendering -/
structure Render.Context where
  /-- Site configuration -/
  config : Config
  /-- The dependency graph -/
  depGraph : Graph
  /-- Current path within the site -/
  path : Array String := #[]
  deriving Inhabited

/-- Mutable state during rendering -/
structure Render.State where
  /-- Generated HTML IDs for deduplication -/
  usedIds : Std.HashSet String := {}
  /-- Errors encountered during rendering -/
  errors : Array String := #[]
  deriving Inhabited

/-- The rendering monad for Blueprint HTML generation -/
abbrev RenderM := StateT Render.State (ReaderT Render.Context IO)

/-- Run a RenderM computation -/
def RenderM.run (m : RenderM α) (ctx : Render.Context) (state : Render.State := {})
    : IO (α × Render.State) :=
  StateT.run m state |>.run ctx

/-- Run a RenderM computation, returning only the result -/
def RenderM.run' (m : RenderM α) (ctx : Render.Context) (state : Render.State := {})
    : IO α :=
  StateT.run' m state |>.run ctx

/-! ## RenderM Operations -/

/-- Get the site configuration -/
def Render.getConfig : RenderM Config := do
  return (← read).config

/-- Get the dependency graph -/
def Render.getDepGraph : RenderM Graph := do
  return (← read).depGraph

/-- Get the current path -/
def Render.getPath : RenderM (Array String) := do
  return (← read).path

/-- Compute relative path to root from current path -/
def Render.pathToRoot : RenderM String := do
  let path ← Render.getPath
  if path.isEmpty then return "./"
  else return String.join (path.toList.map fun _ => "../")

/-- Register an HTML ID and check for duplicates -/
def Render.registerHtmlId (id : String) : RenderM Bool := do
  let state ← get
  if state.usedIds.contains id then
    modify fun s => { s with errors := s.errors.push s!"Duplicate HTML ID: {id}" }
    return false
  else
    modify fun s => { s with usedIds := s.usedIds.insert id }
    return true

/-- Record an error during rendering -/
def Render.logError (msg : String) : RenderM Unit := do
  modify fun s => { s with errors := s.errors.push msg }

/-- Run a computation with a modified path -/
def Render.withPath (newPath : Array String) (m : RenderM α) : RenderM α := do
  withReader (fun ctx => { ctx with path := newPath }) m

/-- Run a computation with an appended path segment -/
def Render.inSubPath (segment : String) (m : RenderM α) : RenderM α := do
  let path ← Render.getPath
  Render.withPath (path.push segment) m

/-! ## Status Rendering -/

namespace Graph.NodeStatus

/-- Convert NodeStatus to CSS class -/
def toCssClass : NodeStatus → String
  | .notReady => "not-ready"
  | .stated => "stated"
  | .ready => "ready"
  | .sorry => "sorry"
  | .proven => "proven"
  | .fullyProven => "fully-proven"
  | .mathlibReady => "mathlib-ready"
  | .inMathlib => "in-mathlib"

/-- Convert NodeStatus to display string -/
def toDisplayString : NodeStatus → String
  | .notReady => "Not Ready"
  | .stated => "Stated"
  | .ready => "Ready"
  | .sorry => "Has Sorry"
  | .proven => "Proven"
  | .fullyProven => "Fully Proven"
  | .mathlibReady => "Mathlib Ready"
  | .inMathlib => "In Mathlib"

end Graph.NodeStatus

/-! ## HTML Construction Helpers -/

/-- Create an HTML element with class attribute -/
def htmlWithClass (tag : String) (cls : String) (content : Html) : Html :=
  .tag tag #[("class", cls)] content

/-- Create a div with class -/
def divClass (cls : String) (content : Html) : Html :=
  htmlWithClass "div" cls content

/-- Create a span with class -/
def spanClass (cls : String) (content : Html) : Html :=
  htmlWithClass "span" cls content

/-- Create a link element -/
def htmlLink (href : String) (content : Html) (cls : Option String := none) : Html :=
  let attrs := #[("href", href)] ++ (cls.map (fun c => #[("class", c)]) |>.getD #[])
  .tag "a" attrs content

/-! ## Node Rendering -/

/-- Render declaration links for a node.

Uses doc-gen4 URL format: `{baseUrl}/{Module}/{Submodule}.html#{declName}`
-/
def renderDeclLinks (names : Array Name) (docgen4Url : Option String) : RenderM Html := do
  if names.isEmpty then return Html.empty
  let links ← names.mapM fun name => do
    let nameStr := name.toString
    let content := Html.text true nameStr
    match docgen4Url with
    | some baseUrl =>
      let url := DocGen4.docUrl baseUrl name
      return .tag "a" #[("href", url), ("class", "decl-link"), ("target", "_blank")] content
    | none =>
      return spanClass "decl-name" content
  return divClass "node-decls" (
    spanClass "decl-label" (Html.text true "Lean: ") ++
    .seq (links.toList.intersperse (Html.text true ", ")).toArray
  )

/-- Render a single blueprint node (plasTeX side-by-side layout) -/
def renderNode (node : NodeInfo) : RenderM Html := do
  let _config ← Render.getConfig
  let _ ← Render.registerHtmlId node.label

  -- Status indicator character
  let statusChar := match node.status with
    | .notReady => "✗"
    | .stated => "○"
    | .ready => "◐"
    | .sorry => "!"
    | .proven => "✓"
    | .fullyProven => "✓"
    | .mathlibReady => "✓"
    | .inMathlib => "✓"

  -- Determine what to display as the label: displayNumber if available, otherwise title or label
  let displayLabel := match node.displayNumber with
    | some num => num
    | none => node.title.getD node.label

  -- Left column: LaTeX statement and proof
  -- Use type-specific CSS classes: definition_thmheading, theorem_thmheading, lemma_thmheading, etc.
  let envTypeLower := node.envType.toLower
  let latexColumn := divClass "sbs-latex-column" (
    -- Theorem heading with type-specific class
    divClass s!"{envTypeLower}_thmheading" (
      spanClass s!"{envTypeLower}_thmcaption" (Html.text true node.envType.capitalize) ++
      Html.text true " " ++
      spanClass s!"{envTypeLower}_thmlabel" (Html.text true displayLabel) ++
      divClass "thm_header_extras" (Html.text true statusChar)
    ) ++
    -- Statement content with type-specific class
    divClass s!"{envTypeLower}_thmcontent" (
      .tag "p" #[] (Html.text false node.statementHtml)
    ) ++
    -- Proof (collapsible)
    (match node.proofHtml with
    | some proofContent =>
      divClass "proof_wrapper proof_inline" (
        divClass "proof_heading" (
          spanClass "proof_caption" (Html.text true "Proof") ++
          spanClass "expand-proof" (Html.text true "▶")
        ) ++
        divClass "proof_content" (
          .tag "p" #[] (Html.text false proofContent)
        )
      )
    | none => Html.empty)
  )

  -- Right column: Lean code with syntax highlighting (signature + proof body separate)
  let leanColumn := divClass "sbs-lean-column" (
    match node.signatureHtml, node.proofBodyHtml with
    | some sigHtml, some proofHtml =>
      -- The lean-code wrapper with hover data attribute
      let hoverAttr := match node.hoverData with
        | some hd => #[("data-lean-hovers", hd)]
        | none => #[]
      .tag "pre" (#[("class", "lean-code hl lean")] ++ hoverAttr) (
        -- Signature (always visible)
        .tag "code" #[("class", "hl lean lean-signature")] (Html.text false sigHtml) ++
        -- Proof body (hidden by default, synced with LaTeX proof toggle)
        .tag "code" #[("class", "hl lean lean-proof-body")] (Html.text false proofHtml)
      )
    | some sigHtml, none =>
      -- Only signature, no proof body
      let hoverAttr := match node.hoverData with
        | some hd => #[("data-lean-hovers", hd)]
        | none => #[]
      .tag "pre" (#[("class", "lean-code hl lean")] ++ hoverAttr) (
        .tag "code" #[("class", "hl lean lean-signature")] (Html.text false sigHtml)
      )
    | none, some proofHtml =>
      -- Only proof body (unusual case)
      let hoverAttr := match node.hoverData with
        | some hd => #[("data-lean-hovers", hd)]
        | none => #[]
      .tag "pre" (#[("class", "lean-code hl lean")] ++ hoverAttr) (
        .tag "code" #[("class", "hl lean lean-proof-body")] (Html.text false proofHtml)
      )
    | none, none =>
      -- Fallback: show declaration names
      .tag "pre" #[("class", "lean-code")] (
        .tag "code" #[("class", "hl lean")] (
          Html.text true (String.intercalate "\n" (node.declNames.map toString).toList)
        )
      )
  )

  -- Main container with plasTeX-compatible classes
  let envClass := s!"theorem-style-{envTypeLower}"
  return .tag "div" #[
    ("id", node.label),
    ("class", s!"{envTypeLower}_thmwrapper sbs-container {envClass}")
  ] (latexColumn ++ leanColumn)

/-! ## Page Rendering -/

/-- Render a page containing multiple nodes -/
def renderPage (title : String) (nodes : Array NodeInfo) : RenderM Html := do
  let nodeHtmls ← nodes.mapM renderNode
  return divClass "blueprint-page" (
    .tag "h1" #[("class", "page-title")] (Html.text true title) ++
    divClass "nodes-container" (.seq nodeHtmls)
  )

/-! ## Progress Statistics -/

/-- Render progress statistics -/
def renderProgress (site : BlueprintSite) : Html :=
  let counts := site.statusCounts
  let total := site.totalNodes
  let pct := site.completionPercentage
  -- Round to integer for display
  let pctRounded := pct.round.toUInt32

  let bar := divClass "progress-bar" (
    .tag "div" #[
      ("class", "progress-fill"),
      ("style", s!"width: {pctRounded}%")
    ] Html.empty
  )

  let stats := divClass "progress-stats" (
    spanClass "stat proven" (Html.text true s!"{counts.proven} proven") ++
    spanClass "stat fully-proven" (Html.text true s!"{counts.fullyProven} fully proven") ++
    spanClass "stat mathlib-ready" (Html.text true s!"{counts.mathlibReady} mathlib ready") ++
    spanClass "stat in-mathlib" (Html.text true s!"{counts.inMathlib} in mathlib") ++
    spanClass "stat sorry" (Html.text true s!"{counts.hasSorry} sorry") ++
    spanClass "stat stated" (Html.text true s!"{counts.stated} stated") ++
    spanClass "stat ready" (Html.text true s!"{counts.ready} ready") ++
    spanClass "stat not-ready" (Html.text true s!"{counts.notReady} not ready") ++
    spanClass "stat total" (Html.text true s!"{total} total")
  )

  divClass "progress-section" (
    .tag "h2" #[] (Html.text true "Progress") ++
    bar ++
    stats ++
    divClass "progress-label" (Html.text true s!"{pctRounded}%")
  )

/-! ## Index Page Rendering -/

/-- Render the main index page -/
def renderIndex (site : BlueprintSite) : RenderM Html := do
  let config ← Render.getConfig

  -- Title section
  let titleSection := divClass "index-header" (
    .tag "h1" #[] (Html.text true config.title) ++
    (match config.githubUrl with
     | some url => htmlLink url (Html.text true "GitHub") (some "github-link")
     | none => Html.empty) ++
    (match config.docgen4Url with
     | some url => htmlLink url (Html.text true "Documentation") (some "docs-link")
     | none => Html.empty)
  )

  -- Progress section
  let progress := renderProgress site

  -- Dependency graph section with embedded SVG (if available)
  let graphSection := DepGraph.graphSection site.depGraphSvg site.depGraphJson

  -- Node list by status (group proven + fullyProven, mathlib ready + in mathlib)
  let provenNodes := site.nodesByStatus .proven ++ site.nodesByStatus .fullyProven
  let mathLibNodes := site.nodesByStatus .mathlibReady ++ site.nodesByStatus .inMathlib
  let statedNodes := site.nodesByStatus .stated

  let nodeList := divClass "node-lists" (
    (if provenNodes.isEmpty then Html.empty else
      divClass "node-list proven" (
        .tag "h3" #[] (Html.text true "Proven") ++
        renderNodeList provenNodes
      )) ++
    (if mathLibNodes.isEmpty then Html.empty else
      divClass "node-list mathlib" (
        .tag "h3" #[] (Html.text true "From Mathlib") ++
        renderNodeList mathLibNodes
      )) ++
    (if statedNodes.isEmpty then Html.empty else
      divClass "node-list stated" (
        .tag "h3" #[] (Html.text true "Stated") ++
        renderNodeList statedNodes
      ))
  )

  return divClass "index-page" (
    titleSection ++
    progress ++
    graphSection ++
    nodeList
  )
where
  renderNodeList (nodes : Array NodeInfo) : Html :=
    .tag "ul" #[("class", "node-index")] (
      .seq (nodes.map fun node =>
        .tag "li" #[] (
          htmlLink s!"#{node.label}" (
            spanClass "node-env" (Html.text true node.envType) ++
            Html.text true " " ++
            Html.text true (node.title.getD node.label)
          ) (some s!"status-{node.status.toCssClass}")
        )
      )
    )

/-! ## Full Site Rendering -/

/-- Render JSON data for the dependency graph (for JavaScript) -/
def renderGraphJson (site : BlueprintSite) : String :=
  let nodes := site.depGraph.nodes.map fun n =>
    "{\"id\":\"" ++ n.id ++ "\",\"label\":\"" ++ n.label ++ "\",\"status\":\"" ++ n.status.toCssClass ++ "\"}"
  let edges := site.depGraph.edges.map fun e =>
    "{\"from\":\"" ++ e.from_ ++ "\",\"to\":\"" ++ e.to ++ "\"}"
  "{\"nodes\":[" ++ ",".intercalate nodes.toList ++ "],\"edges\":[" ++ ",".intercalate edges.toList ++ "]}"

/-- Render the complete site HTML -/
def renderSite (site : BlueprintSite) : RenderM Html := do
  let config ← Render.getConfig
  let toRoot ← Render.pathToRoot

  -- Render all nodes
  let nodesHtml ← site.nodes.mapM renderNode

  -- Render index
  let indexHtml ← renderIndex site

  -- Assemble complete HTML document
  return .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true config.title) ++
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/blueprint.css")] Html.empty ++
      .tag "script" #[("src", s!"{toRoot}assets/verso-code.js")] Html.empty ++
      -- Embed graph data for JavaScript
      .tag "script" #[("id", "graph-data"), ("type", "application/json")]
        (Html.text false (renderGraphJson site))
    ) ++
    .tag "body" #[] (
      indexHtml ++
      divClass "all-nodes" (.seq nodesHtml)
    )
  )

end Runway
