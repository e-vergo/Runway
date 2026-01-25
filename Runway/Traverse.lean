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
    | "stated" => return .stated
    | "proved" => return .proved
    | "notReady" => return .notReady
    | "mathLibOk" => return .mathLibOk
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

/-- Raw declaration artifact from Dress module JSON -/
structure DeclArtifact where
  /-- Pre-rendered HTML for the declaration -/
  html : String
  /-- Base64-encoded HTML (for LaTeX embedding) -/
  htmlBase64 : Option String := none
  /-- Base64-encoded JSON (SubVerso highlighting) -/
  jsonBase64 : Option String := none
  deriving Repr, Inhabited

instance : FromJson DeclArtifact where
  fromJson? j := do
    let html ← j.getObjValAs? String "html"
    let htmlBase64 := (j.getObjValAs? String "htmlBase64").toOption
    let jsonBase64 := (j.getObjValAs? String "jsonBase64").toOption
    return { html, htmlBase64, jsonBase64 }

/-- Node artifact combining graph node with rendered content -/
structure NodeArtifact where
  /-- The graph node metadata -/
  node : Node
  /-- Pre-rendered statement HTML -/
  statementHtml : Option String := none
  /-- Pre-rendered proof HTML -/
  proofHtml : Option String := none
  deriving Repr, Inhabited

/-- Convert a NodeArtifact to a NodeInfo for site building -/
def NodeArtifact.toNodeInfo (art : NodeArtifact) (uses : Array String := #[]) : NodeInfo :=
  { label := art.node.id
    title := some art.node.label
    envType := art.node.envType
    status := art.node.status
    statementHtml := art.statementHtml.getD ""
    proofHtml := art.proofHtml
    declNames := art.node.leanDecls
    uses := uses
    url := art.node.url }

/-- Load the dependency graph from Dress output -/
def loadDepGraph (dressedDir : System.FilePath) : IO Graph := do
  let depGraphPath := dressedDir / "dep-graph.json"
  if ← depGraphPath.pathExists then
    let content ← IO.FS.readFile depGraphPath
    match Json.parse content >>= FromJson.fromJson? with
    | .ok g => return g
    | .error e =>
      IO.eprintln s!"Warning: Failed to parse dep-graph.json: {e}"
      return { nodes := #[], edges := #[] }
  else
    return { nodes := #[], edges := #[] }

/-- Load declaration artifacts from a module JSON file -/
def loadModuleArtifacts (jsonPath : System.FilePath) : IO (HashMap String DeclArtifact) := do
  if ← jsonPath.pathExists then
    let content ← IO.FS.readFile jsonPath
    match Json.parse content with
    | .ok (.obj entries) =>
      let mut result : HashMap String DeclArtifact := {}
      for (name, value) in entries.toArray do
        match FromJson.fromJson? value with
        | .ok art => result := result.insert name art
        | .error _ => pure ()
      return result
    | _ => return {}
  else
    return {}

/-- Scan for module JSON files in the dressed directory -/
def findModuleJsonFiles (dressedDir : System.FilePath) : IO (Array System.FilePath) := do
  if ← dressedDir.pathExists then
    let entries ← dressedDir.readDir
    let jsonFiles := entries.filterMap fun entry =>
      let path := entry.path
      if path.extension == some "json" && path.fileName != some "dep-graph.json" then
        some path
      else
        none
    return jsonFiles
  else
    return #[]

/-- Load all artifacts from a Dress output directory -/
def loadDressedArtifacts (dressedDir : System.FilePath) : IO Blueprint.State := do
  -- Load dependency graph
  let depGraph ← loadDepGraph dressedDir

  -- Build node map from graph
  let mut nodes : HashMap String Node := {}
  for node in depGraph.nodes do
    nodes := nodes.insert node.id node

  -- Load declaration artifacts from all module JSON files
  let moduleFiles ← findModuleJsonFiles dressedDir
  let mut declArtifacts : HashMap String DeclArtifact := {}
  for jsonPath in moduleFiles do
    let moduleArts ← loadModuleArtifacts jsonPath
    for (name, art) in moduleArts.toList do
      declArtifacts := declArtifacts.insert name art

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

/-- Count nodes by status -/
def countNodesByStatus : TraverseM (Nat × Nat × Nat × Nat) := do
  let nodes ← getAllNodes
  let mut stated := 0
  let mut proved := 0
  let mut notReady := 0
  let mut mathLibOk := 0
  for node in nodes do
    match node.status with
    | .stated => stated := stated + 1
    | .proved => proved := proved + 1
    | .notReady => notReady := notReady + 1
    | .mathLibOk => mathLibOk := mathLibOk + 1
  return (stated, proved, notReady, mathLibOk)

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
