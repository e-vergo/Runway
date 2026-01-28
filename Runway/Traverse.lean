/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Std.Data.HashMap
import Std.Data.HashSet
import Runway.Config
import Runway.Genre
import Runway.Graph
import Runway.Site
import Runway.Dress.Load

open Lean (Json FromJson)
open Runway.Dress (decodeBase64String)

/-!
# Document Traversal

Implements the TraverseM monad for loading and accessing Dress artifacts
during Blueprint document rendering.

## Architecture

The traversal phase loads artifacts from Dress output and makes them
available for document rendering. This follows the VersoBlog pattern
of using StateT + ReaderT for traversal.

## Dress Output Format

Dress generates artifacts to `.lake/build/dressed/`:
- Per module: `{Module}.json` - declaration highlighting
- `dep-graph.json` - dependency graph with layout
- `dep-graph.svg` - static SVG rendering

The module JSON contains per-declaration entries:
```json
{
  "MyModule.myTheorem": {
    "html": "<pre class=\"lean\">...",
    "htmlBase64": "...",
    "jsonBase64": "..."
  }
}
```
-/

namespace Runway

open Lean (Name Json ToJson FromJson)
open Std (HashMap HashSet)
open Runway.Graph (NodeStatus Graph Node Edge)

/-! ## FromJson instances for Graph types -/

instance : FromJson NodeStatus where
  fromJson? j := do
    let s ← j.getStr?
    match s with
    | "notReady" => return .notReady
    | "stated" => return .stated
    | "ready" => return .ready
    | "sorry" => return .sorry
    | "proven" => return .proven
    | "fullyProven" => return .fullyProven
    | "mathlibReady" => return .mathlibReady
    | "inMathlib" => return .inMathlib
    | _ => throw s!"Unknown NodeStatus: {s}"

instance : FromJson Node where
  fromJson? j := do
    let id ← j.getObjValAs? String "id"
    let label ← j.getObjValAs? String "label"
    let envType ← j.getObjValAs? String "envType"
    let status ← j.getObjValAs? NodeStatus "status"
    let url ← j.getObjValAs? String "url" <|> pure ""
    let leanDeclsJson ← j.getObjValAs? (Array String) "leanDecls" <|> pure #[]
    let leanDecls := leanDeclsJson.map (·.toName)
    return { id, label, envType, status, url, leanDecls }

instance : FromJson Edge where
  fromJson? j := do
    let from_ ← j.getObjValAs? String "from"
    let to ← j.getObjValAs? String "to"
    return { from_, to }

instance : FromJson Graph where
  fromJson? j := do
    let nodes ← j.getObjValAs? (Array Node) "nodes"
    let edges ← j.getObjValAs? (Array Edge) "edges"
    return { nodes, edges }

/-! ## Artifact Loading -/

/-- Raw declaration artifact from Dress .tex files -/
structure DeclArtifact where
  /-- Declaration name -/
  name : String
  /-- Label for cross-referencing -/
  label : String
  /-- LaTeX statement text for left column (MathJax rendered) -/
  latexStatement : Option String := none
  /-- LaTeX proof text for left column (MathJax rendered) -/
  latexProof : Option String := none
  /-- Lean signature HTML for right column (syntax highlighted) -/
  leanSignatureHtml : Option String := none
  /-- Lean proof body HTML for right column (syntax highlighted) -/
  leanProofBodyHtml : Option String := none
  /-- JSON hover data (decoded from base64) -/
  hoverData : Option String := none
  /-- Dependencies (from \uses{}) -/
  uses : Array String := #[]
  /-- Is Lean-proven? (has \leanok) -/
  leanOk : Bool := false
  deriving Repr, Inhabited

/-- Node artifact combining graph node with rendered content -/
structure NodeArtifact where
  /-- The graph node metadata -/
  node : Node
  /-- Pre-rendered statement HTML -/
  statementHtml : Option String := none
  /-- Pre-rendered proof HTML -/
  proofHtml : Option String := none
  /-- Pre-rendered Lean signature HTML (syntax-highlighted) -/
  signatureHtml : Option String := none
  /-- Pre-rendered Lean proof body HTML (syntax-highlighted) -/
  proofBodyHtml : Option String := none
  /-- Hover data JSON for Tippy.js tooltips -/
  hoverData : Option String := none
  deriving Repr, Inhabited

/-- Convert a NodeArtifact to a NodeInfo for site building -/
def NodeArtifact.toNodeInfo (art : NodeArtifact) (uses : Array String := #[]) : NodeInfo :=
  { label := art.node.id
    title := some art.node.label
    envType := art.node.envType
    status := art.node.status
    statementHtml := art.statementHtml.getD ""
    proofHtml := art.proofHtml
    signatureHtml := art.signatureHtml
    proofBodyHtml := art.proofBodyHtml
    hoverData := art.hoverData
    declNames := art.node.leanDecls
    uses := uses
    url := art.node.url }

/-- Decode base64 string to UTF-8 String -/
private def decodeBase64String (s : String) : Option String := Dress.decodeBase64String s

/-- Check if substr is contained in s -/
private def containsSubstr (s substr : String) : Bool :=
  (s.splitOn substr).length > 1

/-- Find index of substr in s, returns position after the substring -/
private def findSubstrEnd (s substr : String) : Option Nat :=
  let parts := s.splitOn substr
  if parts.length > 1 then
    some (parts[0]!.length + substr.length)
  else
    none

/-- Extract value from a LaTeX command like \cmd{value} -/
private def extractLatexArg (content : String) (cmd : String) : Option String := Id.run do
  -- Find \cmd{
  let cmdPrefix := s!"\\{cmd}\{"
  match findSubstrEnd content cmdPrefix with
  | none => return none
  | some startIdx =>
    let afterCmd := (content.drop startIdx).toString
    -- Find matching }
    let mut depth := 1
    let mut endIdx := 0
    for c in afterCmd.toList do
      if depth == 0 then break
      if c == '{' then depth := depth + 1
      else if c == '}' then depth := depth - 1
      if depth > 0 then endIdx := endIdx + c.utf8Size
    if depth == 0 then
      return some (afterCmd.take endIdx).toString
    else
      return none

/-- Extract text between two markers, returning the text after start up to (not including) end -/
private def extractBetween (content : String) (startMarker : String) (endMarker : String) : Option String := Id.run do
  let parts := content.splitOn startMarker
  if parts.length < 2 then return none
  let afterStart := parts[1]!
  let endParts := afterStart.splitOn endMarker
  if endParts.isEmpty then return none
  return some endParts[0]!.trimAscii.toString

/-- Strip \uses{...} commands from LaTeX text -/
private def stripUsesCommand (content : String) : String := Id.run do
  let mut result := content
  -- Keep stripping \uses{...} until none remain
  while containsSubstr result "\\uses{" do
    let parts := result.splitOn "\\uses{"
    if parts.length < 2 then break
    let beforeUses := parts[0]!
    let afterUses := parts[1]!
    -- Find matching }
    let mut depth := 1
    let mut endIdx := 0
    for c in afterUses.toList do
      if depth == 0 then break
      if c == '{' then depth := depth + 1
      else if c == '}' then depth := depth - 1
      if depth > 0 then endIdx := endIdx + c.utf8Size
      else endIdx := endIdx + 1  -- Include the closing }
    let afterClosingBrace := afterUses.drop endIdx
    result := beforeUses ++ afterClosingBrace
  return result.trimAscii.toString

/-- Convert LaTeX list environments to HTML.
    Converts \begin{itemize}...\end{itemize} to <ul>...<li>...</li>...</ul>
    and \begin{enumerate}...\end{enumerate} to <ol>...<li>...</li>...</ol> -/
private def convertLatexListsToHtml (content : String) : String := Id.run do
  let mut result := content

  -- Convert itemize environments
  result := result.replace "\\begin{itemize}" "<ul>"
  result := result.replace "\\end{itemize}" "</ul>"

  -- Convert enumerate environments
  result := result.replace "\\begin{enumerate}" "<ol>"
  result := result.replace "\\end{enumerate}" "</ol>"

  -- Convert \item to <li>
  -- We need to close previous <li> before opening a new one
  -- Strategy: replace \item with </li><li>, then fix up first/last
  if containsSubstr result "\\item" then
    let parts := result.splitOn "\\item"
    if parts.length > 1 then
      let mut newResult := parts[0]!
      for i in [1:parts.length] do
        -- Add <li> before each item content
        newResult := newResult ++ "<li>" ++ parts[i]!.trimAscii
      result := newResult
      -- Close the last <li> before </ul> or </ol>
      result := result.replace "<li></ul>" "</ul>"
      result := result.replace "<li></ol>" "</ol>"
      -- Handle case where </ul> or </ol> comes after item content
      result := result.replace "</ul>" "</li></ul>"
      result := result.replace "</ol>" "</li></ol>"
      -- Fix double closing tags that might result
      result := result.replace "</li></li>" "</li>"

  return result

/-- Parse a decl.tex file and extract artifact data -/
def parseDeclTex (content : String) : DeclArtifact := Id.run do
  let mut artifact : DeclArtifact := { name := "", label := "" }

  -- Extract label from \label{...}
  if let some label := extractLatexArg content "label" then
    artifact := { artifact with label := label }

  -- Extract Lean name from \lean{...}
  if let some name := extractLatexArg content "lean" then
    artifact := { artifact with name := name }

  -- Extract and decode Lean signature HTML from \leansignaturesourcehtml{base64}
  if let some sigBase64 := extractLatexArg content "leansignaturesourcehtml" then
    if let some sigHtml := decodeBase64String sigBase64 then
      artifact := { artifact with leanSignatureHtml := some sigHtml }

  -- Extract and decode Lean proof body HTML from \leanproofsourcehtml{base64}
  if let some proofBase64 := extractLatexArg content "leanproofsourcehtml" then
    if let some proofHtml := decodeBase64String proofBase64 then
      artifact := { artifact with leanProofBodyHtml := some proofHtml }

  -- Extract hover data from \leanhoverdata{base64}
  if let some hoverBase64 := extractLatexArg content "leanhoverdata" then
    if let some hoverJson := decodeBase64String hoverBase64 then
      artifact := { artifact with hoverData := some hoverJson }

  -- Extract dependencies from \uses{dep1,dep2,...}
  if let some usesStr := extractLatexArg content "uses" then
    let deps := usesStr.splitOn "," |>.map (fun s => String.ofList (s.toList.dropWhile Char.isWhitespace)) |>.filter (!·.isEmpty)
    artifact := { artifact with uses := deps.toArray }

  -- Check for \leanok
  let hasLeanOk := containsSubstr content "\\leanok"
  artifact := { artifact with leanOk := hasLeanOk }

  -- Extract LaTeX statement text (between last \leanok or \uses{...} and \end{theorem/definition/lemma/...})
  -- The statement is the text after all the metadata commands, before \end{...}
  -- Try to find text after the last metadata marker before \end{theorem} etc.
  let theoremEnd := "\\end{theorem}"
  let definitionEnd := "\\end{definition}"
  let lemmaEnd := "\\end{lemma}"
  let propositionEnd := "\\end{proposition}"
  let corollaryEnd := "\\end{corollary}"
  let exampleEnd := "\\end{example}"
  let remarkEnd := "\\end{remark}"

  -- Find which end marker exists
  let endMarker :=
    if containsSubstr content theoremEnd then theoremEnd
    else if containsSubstr content definitionEnd then definitionEnd
    else if containsSubstr content lemmaEnd then lemmaEnd
    else if containsSubstr content propositionEnd then propositionEnd
    else if containsSubstr content corollaryEnd then corollaryEnd
    else if containsSubstr content exampleEnd then exampleEnd
    else if containsSubstr content remarkEnd then remarkEnd
    else ""

  if !endMarker.isEmpty then
    -- Extract text between \leanok (in theorem block) and end marker
    -- First, find the theorem block (before \begin{proof})
    let proofStart := "\\begin{proof}"
    let theoremBlock := if containsSubstr content proofStart then
      (content.splitOn proofStart)[0]!
    else
      content

    -- Find the LaTeX statement: text after \leanok (first occurrence in theorem block) until end marker
    -- But we need to handle the case where \leanok comes after \uses{}
    if let some text := extractBetween theoremBlock "\\leanok\n" endMarker then
      artifact := { artifact with latexStatement := some (convertLatexListsToHtml (stripUsesCommand text)) }
    else if let some text := extractBetween theoremBlock "\\leanok" endMarker then
      artifact := { artifact with latexStatement := some (convertLatexListsToHtml (stripUsesCommand text)) }

  -- Extract LaTeX proof text (between \leanok in proof block and \end{proof})
  if containsSubstr content "\\begin{proof}" then
    let proofEnd := "\\end{proof}"
    if let some proofBlock := extractBetween content "\\begin{proof}" proofEnd then
      -- Find text after \leanok in the proof block
      if let some proofText := extractBetween proofBlock "\\leanok\n" "" then
        artifact := { artifact with latexProof := some (convertLatexListsToHtml (stripUsesCommand proofText)) }
      else if let some proofText := extractBetween proofBlock "\\leanok" "" then
        artifact := { artifact with latexProof := some (convertLatexListsToHtml (stripUsesCommand proofText)) }
      else
        -- No \leanok in proof, use whole proof block
        artifact := { artifact with latexProof := some (convertLatexListsToHtml (stripUsesCommand proofBlock)) }

  return artifact

/-- Recursively find all decl.tex files in a directory -/
partial def findDeclTexFiles (dir : System.FilePath) : IO (Array System.FilePath) := do
  if !(← dir.pathExists) then return #[]

  let entries ← dir.readDir
  let mut results : Array System.FilePath := #[]
  for entry in entries do
    let path := entry.path
    if ← path.isDir then
      -- Recursively search subdirectories
      let subResults ← findDeclTexFiles path
      results := results ++ subResults
    else if path.fileName == some "decl.tex" then
      results := results.push path
  return results

/-- Load all decl.tex artifacts from dressed directory -/
def loadDeclArtifacts (dressedDir : System.FilePath) : IO (HashMap String DeclArtifact) := do
  let texFiles ← findDeclTexFiles dressedDir
  let mut artifacts : HashMap String DeclArtifact := {}

  for texPath in texFiles do
    let content ← IO.FS.readFile texPath
    let artifact := parseDeclTex content
    -- Use label as key (removing colon prefix like "lem:" -> "lem-...")
    let key := artifact.label.replace ":" "-"
    artifacts := artifacts.insert key artifact

  return artifacts

/-- Build a dependency graph from decl.tex artifacts.
    Creates nodes from artifacts and edges from \uses{} dependencies. -/
def buildGraphFromArtifacts (artifacts : HashMap String DeclArtifact) : Graph := Id.run do
  let mut nodes : Array Node := #[]
  let mut edges : Array Edge := #[]

  for (key, art) in artifacts.toArray do
    -- Create node
    let status : NodeStatus := if art.leanOk then .proven else .stated
    -- Determine env type from label prefix
    let envType :=
      if art.label.startsWith "thm:" || art.label.startsWith "thm-" then "theorem"
      else if art.label.startsWith "lem:" || art.label.startsWith "lem-" then "theorem"
      else if art.label.startsWith "prop:" || art.label.startsWith "prop-" then "proposition"
      else if art.label.startsWith "cor:" || art.label.startsWith "cor-" then "corollary"
      else if art.label.startsWith "def:" then "definition"
      else if key.startsWith "def-" then "definition"
      else "theorem"
    let node : Node := {
      id := key
      label := if art.name.isEmpty then key else art.name
      envType
      status
      url := s!"#node-{key}"
      leanDecls := if art.name.isEmpty then #[] else #[art.name.toName]
    }
    nodes := nodes.push node

    -- Create edges from uses
    for dep in art.uses do
      let depKey := dep.replace ":" "-"
      edges := edges.push { from_ := key, to := depKey }

  return { nodes, edges }

/-- Load the dependency graph from Dress output, falling back to building from artifacts -/
def loadDepGraph (dressedDir : System.FilePath) : IO Graph := do
  let depGraphPath := dressedDir / "dep-graph.json"
  if ← depGraphPath.pathExists then
    let content ← IO.FS.readFile depGraphPath
    match Json.parse content >>= (FromJson.fromJson? : Json → Except String Graph) with
    | .ok g =>
      -- Check if the graph has nodes; if empty, fall back to building from artifacts
      if g.nodes.isEmpty then
        IO.eprintln s!"Note: dep-graph.json exists but has no nodes, building from artifacts..."
        let artifacts ← loadDeclArtifacts dressedDir
        return buildGraphFromArtifacts artifacts
      return g
    | .error e =>
      IO.eprintln s!"Warning: Failed to parse dep-graph.json: {e}"
      let artifacts ← loadDeclArtifacts dressedDir
      return buildGraphFromArtifacts artifacts
  else
    IO.eprintln s!"Note: dep-graph.json not found at {depGraphPath}, building from artifacts..."
    let artifacts ← loadDeclArtifacts dressedDir
    return buildGraphFromArtifacts artifacts

/-- Load all artifacts from a Dress output directory -/
def loadDressedArtifacts (dressedDir : System.FilePath) : IO Blueprint.State := do
  -- Load dependency graph
  let depGraph ← loadDepGraph dressedDir

  -- Build node map from graph
  let mut nodes : HashMap String Node := {}
  for node in depGraph.nodes do
    nodes := nodes.insert node.id node

  -- If no nodes from graph, build from tex artifacts
  if nodes.isEmpty then
    let artifacts ← loadDeclArtifacts dressedDir
    IO.eprintln s!"Loaded {artifacts.size} artifacts from decl.tex files"
    -- We'll need to create nodes from artifacts when graph isn't available

  return { nodes, usedRefs := {}, errors := {}, usedIds := {} }

/-! ## TraverseM Monad -/

/-- The traversal monad for Blueprint document processing.
    Combines state (nodes, refs, errors) with read-only context (config, graph). -/
abbrev TraverseM := StateT Blueprint.State (ReaderT Blueprint.Context IO)

/-- Run a TraverseM computation -/
def TraverseM.run (m : TraverseM α) (ctx : Blueprint.Context) (state : Blueprint.State := {})
    : IO (α × Blueprint.State) :=
  StateT.run m state |>.run ctx

/-- Run a TraverseM computation, returning only the result -/
def TraverseM.run' (m : TraverseM α) (ctx : Blueprint.Context) (state : Blueprint.State := {})
    : IO α :=
  StateT.run' m state |>.run ctx

/-- Run a TraverseM computation, returning only the final state -/
def TraverseM.exec (m : TraverseM α) (ctx : Blueprint.Context) (state : Blueprint.State := {})
    : IO Blueprint.State := do
  let (_, s) ← StateT.run m state |>.run ctx
  return s

/-! ## TraverseM Operations -/

/-- Get the site configuration -/
def getConfig : TraverseM Config := do
  return (← read).config

/-- Get the dependency graph -/
def getDepGraph : TraverseM Graph := do
  return (← read).depGraph

/-- Lookup a node by its label/id -/
def getNode? (label : String) : TraverseM (Option Node) := do
  return (← get).nodes.get? label

/-- Get all loaded nodes -/
def getAllNodes : TraverseM (Array Node) := do
  return (← get).nodes.values.toArray

/-- Mark a node as referenced (for tracking which nodes are used) -/
def markNodeUsed (label : String) : TraverseM Unit := do
  modify fun s => { s with usedRefs := s.usedRefs.insert label }

/-- Check if a node has been referenced -/
def isNodeUsed (label : String) : TraverseM Bool := do
  return (← get).usedRefs.contains label

/-- Get all referenced node labels -/
def getUsedNodeLabels : TraverseM (HashSet String) := do
  return (← get).usedRefs

/-- Record an error during traversal -/
def recordError (msg : String) : TraverseM Unit := do
  modify fun s => { s with errors := s.errors.insert msg }

/-- Get all recorded errors -/
def getErrors : TraverseM (HashSet String) := do
  return (← get).errors

/-- Check if any errors were recorded -/
def hasErrors : TraverseM Bool := do
  return !(← get).errors.isEmpty

/-- Register an HTML ID (for duplicate detection) -/
def registerHtmlId (id : String) : TraverseM Bool := do
  let state ← get
  if state.usedIds.contains id then
    recordError s!"Duplicate HTML ID: {id}"
    return false
  else
    modify fun s => { s with usedIds := s.usedIds.insert id }
    return true

/-- Get nodes that depend on a given node -/
def getDependents (label : String) : TraverseM (Array String) := do
  let graph ← getDepGraph
  return (graph.inEdges label).map (·.from_)

/-- Get nodes that a given node depends on -/
def getDependencies (label : String) : TraverseM (Array String) := do
  let graph ← getDepGraph
  return (graph.outEdges label).map (·.to)

/-- Get nodes by status -/
def getNodesByStatus (status : NodeStatus) : TraverseM (Array Node) := do
  let nodes ← getAllNodes
  return nodes.filter (·.status == status)

/-- Count nodes by status (returns counts for all 8 statuses) -/
def countNodesByStatus : TraverseM StatusCounts := do
  let nodes ← getAllNodes
  let mut counts : StatusCounts := {}
  for node in nodes do
    match node.status with
    | .notReady => counts := { counts with notReady := counts.notReady + 1 }
    | .stated => counts := { counts with stated := counts.stated + 1 }
    | .ready => counts := { counts with ready := counts.ready + 1 }
    | .sorry => counts := { counts with hasSorry := counts.hasSorry + 1 }
    | .proven => counts := { counts with proven := counts.proven + 1 }
    | .fullyProven => counts := { counts with fullyProven := counts.fullyProven + 1 }
    | .mathlibReady => counts := { counts with mathlibReady := counts.mathlibReady + 1 }
    | .inMathlib => counts := { counts with inMathlib := counts.inMathlib + 1 }
  return { counts with total := nodes.size }

/-! ## Site Building -/

/-- Create a Blueprint.Context from loaded artifacts -/
def mkContext (config : Config) (depGraph : Graph) : Blueprint.Context :=
  { config, depGraph }

/-- Create initial Blueprint.State from dependency graph nodes -/
def mkStateFromGraph (graph : Graph) : Blueprint.State := Id.run do
  let mut nodes : HashMap String Node := {}
  for node in graph.nodes do
    nodes := nodes.insert node.id node
  return { nodes, usedRefs := {}, errors := {}, usedIds := {} }

/-- Load artifacts and create context/state for traversal -/
def loadForTraversal (config : Config) (dressedDir : System.FilePath)
    : IO (Blueprint.Context × Blueprint.State) := do
  let depGraph ← loadDepGraph dressedDir
  let ctx := mkContext config depGraph
  let state := mkStateFromGraph depGraph
  return (ctx, state)

end Runway
