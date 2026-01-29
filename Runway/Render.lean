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
import Dress.Render

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

/-! ## Status Conversion -/

/-- Convert Runway.Graph.NodeStatus to Dress.Graph.NodeStatus for rendering -/
def toDressStatus : Runway.Graph.NodeStatus → Dress.Graph.NodeStatus
  | .notReady => .notReady
  | .stated => .stated
  | .ready => .ready
  | .sorry => .sorry
  | .proven => .proven
  | .fullyProven => .fullyProven
  | .mathlibReady => .mathlibReady
  | .inMathlib => .inMathlib

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

/-- Convert NodeStatus to color hex code -/
def toColor : NodeStatus → String
  | .notReady => "#F4A460"
  | .stated => "#FFD700"
  | .ready => "#20B2AA"
  | .sorry => "#8B0000"
  | .proven => "#90EE90"
  | .fullyProven => "#228B22"
  | .mathlibReady => "#4169E1"
  | .inMathlib => "#191970"

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
  let _ ← Render.registerHtmlId node.label

  -- Convert NodeInfo to SbsData for consolidated rendering
  let sbsData : Dress.Render.SbsData := {
    id := node.label
    label := node.displayNumber.getD (node.title.getD node.label)
    displayNumber := node.displayNumber
    envType := node.envType.toLower
    status := toDressStatus node.status
    statementHtml := node.statementHtml
    proofHtml := node.proofHtml
    signatureHtml := node.signatureHtml
    proofBodyHtml := node.proofBodyHtml
    hoverData := node.hoverData
    declNames := node.declNames
  }

  -- Render using the consolidated function
  let html := Dress.Render.renderSideBySide sbsData .blueprint
  return Html.text false html

/-! ## Modal Rendering for Dependency Graph -/

/-- Render a node as a modal for the dependency graph (wrapping sbs-container in modal structure) -/
def renderNodeModal (node : NodeInfo) : RenderM Html := do
  let sbsContent ← renderNode node
  -- Link to the node's location in the blueprint (use chapter page if available, otherwise index)
  let linkUrl := s!"#{node.label}"
  return DepGraph.wrapInModal node.label sbsContent linkUrl

/-- Generate all modals for the dependency graph page -/
def renderAllModals (nodes : Array NodeInfo) : RenderM Html := do
  let modals ← nodes.mapM renderNodeModal
  return .seq modals

/-! ## Page Rendering -/

/-- Render a page containing multiple nodes -/
def renderPage (title : String) (nodes : Array NodeInfo) : RenderM Html := do
  let nodeHtmls ← nodes.mapM renderNode
  return divClass "blueprint-page" (
    .tag "h1" #[("class", "page-title")] (Html.text true title) ++
    divClass "nodes-container" (.seq nodeHtmls)
  )

/-! ## Progress Statistics -/

/-- Render a single pie slice as an SVG path arc.
    Uses cumulative offset for positioning. Returns the path element and updated offset.
    The offset parameter tracks cumulative percentage used so far.
    dashOffset uses negative value to position the start of each slice correctly. -/
private def renderPieSlice (count : Nat) (total : Nat) (offset : Float) (color : String) : Html × Float :=
  if count == 0 || total == 0 then
    (Html.empty, offset)
  else
    let pct := (count.toFloat / total.toFloat) * 100.0
    -- For stroke-dasharray technique on a circle with r=8 (circumference = 2*pi*8 ≈ 50.27)
    let circumference := 50.27
    let dashLen := pct * circumference / 100.0
    -- Use negative offset to position the start of this slice at the correct rotation
    let dashOffset := -offset * circumference / 100.0
    let slice := .tag "circle" #[
      ("cx", "16"), ("cy", "16"), ("r", "8"),
      ("fill", "transparent"),
      ("stroke", color),
      ("stroke-width", "16"),
      ("stroke-dasharray", s!"{dashLen} {circumference}"),
      ("stroke-dashoffset", s!"{dashOffset}"),
      ("transform", "rotate(-90 16 16)")
    ] Html.empty
    (slice, offset + pct)

/-- Render a legend item with colored swatch -/
private def renderLegendItem (count : Nat) (label : String) (cssClass : String) : Html :=
  divClass "legend-item" (
    .tag "span" #[("class", s!"legend-swatch {cssClass}")] Html.empty ++
    .tag "span" #[] (Html.text true s!"{count} {label}")
  )

/-- Render progress statistics as stats box with pie chart + legend -/
def renderProgress (site : BlueprintSite) : Html :=
  let counts := site.statusCounts
  let total := site.totalNodes

  if total == 0 then Html.empty
  else
    -- Build pie slices with cumulative offset
    -- Order: not-ready, stated, ready, sorry, proven, fully-proven, mathlib-ready, in-mathlib
    let (slice1, off1) := renderPieSlice counts.notReady total 0.0 "#F4A460"
    let (slice2, off2) := renderPieSlice counts.stated total off1 "#FFD700"
    let (slice3, off3) := renderPieSlice counts.ready total off2 "#20B2AA"
    let (slice4, off4) := renderPieSlice counts.hasSorry total off3 "#8B0000"
    let (slice5, off5) := renderPieSlice counts.proven total off4 "#90EE90"
    let (slice6, off6) := renderPieSlice counts.fullyProven total off5 "#228B22"
    let (slice7, off7) := renderPieSlice counts.mathlibReady total off6 "#4169E1"
    let (slice8, _) := renderPieSlice counts.inMathlib total off7 "#191970"

    let pieSlices := .tag "svg" #[
      ("class", "stats-pie"),
      ("viewBox", "0 0 32 32")
    ] (slice1 ++ slice2 ++ slice3 ++ slice4 ++ slice5 ++ slice6 ++ slice7 ++ slice8)

    let legendItems :=
      renderLegendItem counts.notReady "not ready" "not-ready" ++
      renderLegendItem counts.stated "stated" "stated" ++
      renderLegendItem counts.ready "ready" "ready" ++
      renderLegendItem counts.hasSorry "sorry" "sorry" ++
      renderLegendItem counts.proven "proven" "proven" ++
      renderLegendItem counts.fullyProven "fully proven" "fully-proven" ++
      renderLegendItem counts.mathlibReady "mathlib ready" "mathlib-ready" ++
      renderLegendItem counts.inMathlib "in mathlib" "in-mathlib"

    -- Compute stats for the two-column layout
    let provenCount := counts.proven + counts.fullyProven
    let sorryCount := counts.hasSorry
    let otherCount := counts.notReady + counts.stated + counts.ready +
                      counts.mathlibReady + counts.inMathlib

    -- Compute attention counts from site.nodes
    let blockedCount := site.nodes.filter (·.blocked.isSome) |>.size
    let issueCount := site.nodes.filter (·.potentialIssue.isSome) |>.size
    let debtCount := site.nodes.filter (·.technicalDebt.isSome) |>.size

    divClass "stats-box" (
      -- Title
      divClass "stats-title" (Html.text true "Progress") ++
      -- Separator
      divClass "stats-separator" Html.empty ++
      -- Pie + Legend row
      divClass "stats-main" (
        divClass "stats-pie-container" pieSlices ++
        divClass "stats-legend-separator" Html.empty ++
        divClass "stats-legend" legendItems
      ) ++
      -- Separator
      divClass "stats-separator" Html.empty ++
      -- Stats details with two-column layout
      divClass "stats-details" (
        -- Total count header
        divClass "stats-total" (Html.text true s!"{total} declarations") ++
        -- Two-column layout
        divClass "stats-columns" (
          -- Left column: Completion stats
          divClass "stats-column" (
            divClass "stats-column-header" (Html.text true "Completion") ++
            divClass "stats-row" (
              spanClass "stats-value" (Html.text true s!"{provenCount}") ++
              spanClass "stats-label" (Html.text true " proven")
            ) ++
            divClass "stats-row" (
              spanClass "stats-value" (Html.text true s!"{sorryCount}") ++
              spanClass "stats-label" (Html.text true " sorry")
            ) ++
            divClass "stats-row" (
              spanClass "stats-value" (Html.text true s!"{otherCount}") ++
              spanClass "stats-label" (Html.text true " other")
            )
          ) ++
          -- Right column: Attention stats
          divClass "stats-column" (
            divClass "stats-column-header" (Html.text true "Attention") ++
            divClass "stats-row" (
              spanClass "stats-value" (Html.text true s!"{blockedCount}") ++
              spanClass "stats-label" (Html.text true " blocked")
            ) ++
            divClass "stats-row" (
              spanClass "stats-value" (Html.text true s!"{issueCount}") ++
              spanClass "stats-label" (Html.text true " issues")
            ) ++
            divClass "stats-row" (
              spanClass "stats-value" (Html.text true s!"{debtCount}") ++
              spanClass "stats-label" (Html.text true " tech debt")
            )
          )
        )
      )
    )

/-- Render Key Declarations panel (top-right) with mini side-by-side previews -/
def renderKeyDeclarations (site : BlueprintSite) : Html :=
  let keyNodes := site.nodes.filter (·.keyDeclaration)
  divClass "stats-box key-declarations" (
    divClass "stats-title" (Html.text true "Key Declarations") ++
    divClass "stats-separator" Html.empty ++
    if keyNodes.isEmpty then
      divClass "stats-empty" (Html.text true "No key declarations marked")
    else
      divClass "key-declarations-list" (
        .seq (keyNodes.map fun node =>
          let statusColor := node.status.toColor
          divClass "key-declaration-item" (
            -- Status dot
            .tag "span" #[("class", "status-dot"), ("style", s!"background:{statusColor}")] Html.empty ++
            -- Container with clickable title and selectable preview
            divClass "key-declaration-link" (
              -- Clickable title link
              .tag "a" #[("href", node.url), ("class", "key-declaration-title-link")] (
                Html.text true (node.title.getD node.label)
              ) ++
              -- Preview content (NOT wrapped in anchor - allows text selection)
              divClass "key-declaration-preview" (
                -- Left: LaTeX statement (collapsed, no toggle)
                divClass "kd-latex" (
                  spanClass "kd-env" (Html.text true node.envType.capitalize) ++
                  Html.text true " " ++
                  spanClass "kd-label" (Html.text true (node.displayNumber.getD node.label)) ++
                  divClass "kd-statement" (Html.text false node.statementHtml)
                ) ++
                -- Right: Lean signature with hover data (matches renderNode pattern)
                divClass "kd-lean" (
                  match node.signatureHtml with
                  | some sig =>
                    let hoverAttr := match node.hoverData with
                      | some hd => #[("data-lean-hovers", hd)]
                      | none => #[]
                    .tag "pre" (#[("class", "lean-code hl lean")] ++ hoverAttr) (
                      .tag "code" #[("class", "hl lean")] (Html.text false sig)
                    )
                  | none => Html.text true (node.declNames.map toString |>.toList |> ", ".intercalate)
                )
              )
            )
          )
        )
      )
  )

/-- Render Messages panel (bottom-left) -/
def renderMessages (site : BlueprintSite) : Html :=
  let nodesWithMessages := site.nodes.filter (·.message.isSome)
  divClass "stats-box messages" (
    divClass "stats-title" (Html.text true "Messages") ++
    divClass "stats-separator" Html.empty ++
    if nodesWithMessages.isEmpty then
      divClass "stats-empty" (Html.text true "No messages")
    else
      .tag "ul" #[("class", "dashboard-list")] (
        .seq (nodesWithMessages.map fun node =>
          let msg := node.message.getD ""
          .tag "li" #[] (
            .tag "a" #[("href", node.url)] (Html.text true (node.title.getD node.label)) ++
            .tag "span" #[("class", "message-text")] (Html.text true s!" — {msg}")
          )
        )
      )
  )

/-- Render Project Notes panel (bottom-right) -/
def renderProjectNotes (site : BlueprintSite) : Html :=
  let messageNodes := site.nodes.filter (·.message.isSome)
  let priorityNodes := site.nodes.filter (·.priorityItem)
  let blockedNodes := site.nodes.filter (·.blocked.isSome)
  let issueNodes := site.nodes.filter (·.potentialIssue.isSome)
  let debtNodes := site.nodes.filter (·.technicalDebt.isSome)
  let miscNodes := site.nodes.filter (·.misc.isSome)

  let hasMessages := !messageNodes.isEmpty
  let hasOtherNotes := !priorityNodes.isEmpty || !blockedNodes.isEmpty ||
                       !issueNodes.isEmpty || !debtNodes.isEmpty || !miscNodes.isEmpty
  let hasContent := hasMessages || hasOtherNotes

  divClass "stats-box project-notes" (
    divClass "stats-title" (Html.text true "Project Notes") ++
    divClass "stats-separator" Html.empty ++
    if !hasContent then
      divClass "stats-empty" (Html.text true "No project notes")
    else
      divClass "notes-layout" (
        -- Left column: Messages (always show even if empty, to reserve space)
        divClass "notes-messages" (
          .tag "h4" #[] (Html.text true "Messages") ++
          if messageNodes.isEmpty then
            divClass "notes-empty" (Html.text true "No messages")
          else
            .tag "ul" #[("class", "notes-list")] (
              .seq (messageNodes.map fun node =>
                .tag "li" #[] (
                  .tag "a" #[("href", node.fullUrl)] (Html.text true (node.title.getD node.label)) ++
                  divClass "note-content" (Html.text true (node.message.getD ""))
                )
              )
            )
        ) ++
        -- Right column: Other notes
        divClass "notes-other" (
          renderNoteSection "Priority Items" priorityNodes (fun _ => "") ++
          renderNoteSection "Blocked" blockedNodes (·.blocked.getD "") ++
          renderNoteSection "Potential Issues" issueNodes (·.potentialIssue.getD "") ++
          renderNoteSection "Technical Debt" debtNodes (·.technicalDebt.getD "") ++
          renderNoteSection "Misc" miscNodes (·.misc.getD "")
        )
      )
  )
where
  renderNoteSection (title : String) (nodes : Array NodeInfo) (getText : NodeInfo → String) : Html :=
    if nodes.isEmpty then Html.empty
    else
      divClass "note-section" (
        .tag "h4" #[] (Html.text true title) ++
        .tag "ul" #[("class", "notes-list")] (
          .seq (nodes.map fun node =>
            let text := getText node
            .tag "li" #[] (
              .tag "a" #[("href", node.fullUrl)] (Html.text true (node.title.getD node.label)) ++
              (if text.isEmpty then Html.empty else
                spanClass "note-text" (Html.text true s!" — {text}"))
            )
          )
        )
      )

/-- Render the Checks tile showing graph validation results -/
def renderChecks (site : BlueprintSite) : Html :=
  let checks := site.checks.getD {
    isConnected := true
    numComponents := 1
    componentSizes := #[0]
    cycles := #[]
  }

  -- Connectedness check
  let connectednessClass := if checks.isConnected then "check-pass" else "check-fail"
  let connectednessIcon := if checks.isConnected then "✓" else "✗"
  let connectednessText := if checks.isConnected
    then "Graph is connected"
    else "Graph is disconnected"

  -- Cycles check
  let cyclesClass := if checks.cycles.isEmpty then "check-pass" else "check-fail"
  let cyclesIcon := if checks.cycles.isEmpty then "✓" else "✗"
  let cyclesText := if checks.cycles.isEmpty
    then "No circular dependencies"
    else s!"{checks.cycles.size} cycle(s) detected"

  -- Render the tile
  divClass "stats-box checks-tile" (
    divClass "stats-title" (Html.text true "Graph Checks") ++
    divClass "stats-separator" Html.empty ++
    divClass "checks-list" (
      divClass s!"check-item {connectednessClass}" (
        spanClass "check-icon" (Html.text true connectednessIcon) ++
        spanClass "check-text" (Html.text true connectednessText)
      ) ++
      divClass s!"check-item {cyclesClass}" (
        spanClass "check-icon" (Html.text true cyclesIcon) ++
        spanClass "check-text" (Html.text true cyclesText)
      ) ++
      -- If there are cycles, list them
      (if checks.cycles.isEmpty then Html.text true ""
       else divClass "cycles-detail" (
         .tag "ul" #[] (
           checks.cycles.foldl (fun acc cycle =>
             acc ++ .tag "li" #[] (Html.text true (", ".intercalate cycle.toList))
           ) (Html.text true "")
         )
       )) ++
      -- Placeholder: Kernel Verification
      divClass "check-item check-pending" (
        spanClass "check-icon" (Html.text true "○") ++
        spanClass "check-text" (Html.text true "Kernel Verification") ++
        spanClass "check-status" (Html.text true "Not implemented")
      ) ++
      -- Placeholder: Soundness Checks
      divClass "check-item check-pending" (
        spanClass "check-icon" (Html.text true "○") ++
        spanClass "check-text" (Html.text true "Soundness Checks") ++
        spanClass "check-status" (Html.text true "Not implemented")
      )
    )
  )

/-- Render the dashboard grid -/
def renderDashboard (site : BlueprintSite) : Html :=
  divClass "dashboard-grid" (
    -- Top row: Progress (fixed width) + Key Theorems (fills remaining space)
    divClass "dashboard-row top-row" (
      -- Left column: Progress + Checks stacked vertically
      divClass "dashboard-cell progress-cell" (
        renderProgress site ++
        renderChecks site
      ) ++
      divClass "dashboard-cell key-declarations-cell" (renderKeyDeclarations site)
    ) ++
    -- Bottom row: Project Notes spanning full width
    divClass "dashboard-row bottom-row" (
      divClass "dashboard-cell notes-cell" (renderProjectNotes site)
    )
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

  -- Dashboard section (2x2 grid with progress, key theorems, messages, notes)
  let dashboard := renderDashboard site

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
    dashboard ++
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
