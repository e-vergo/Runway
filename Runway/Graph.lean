/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Dependency Graph Types

Core types for dependency graph construction and visualization.
These mirror the types in Dress.Graph.Types for compatibility.
-/

namespace Runway.Graph

open Lean (Name ToJson FromJson)

/-- Node status for visualization coloring -/
inductive NodeStatus where
  | stated       -- Has statement, no Lean
  | proved       -- leanOk = true (has proof)
  | notReady     -- notReady = true
  | mathLibOk    -- Proved by Mathlib reference
  deriving Repr, Inhabited, BEq, DecidableEq

instance : ToJson NodeStatus where
  toJson
    | .stated => .str "stated"
    | .proved => .str "proved"
    | .notReady => .str "notReady"
    | .mathLibOk => .str "mathLibOk"

instance : FromJson NodeStatus where
  fromJson? j := do
    let s ← j.getStr?
    match s with
    | "stated" => return .stated
    | "proved" => return .proved
    | "notReady" => return .notReady
    | "mathLibOk" => return .mathLibOk
    | _ => throw s!"Unknown NodeStatus: {s}"

/-- A node in the dependency graph -/
structure Node where
  /-- Unique identifier (label) -/
  id : String
  /-- Display label (theorem type + number) -/
  label : String
  /-- Environment type (theorem, lemma, etc.) -/
  envType : String
  /-- Current status -/
  status : NodeStatus
  /-- URL to the node's section in the HTML -/
  url : String
  /-- Associated Lean declaration names -/
  leanDecls : Array Name
  deriving Repr, Inhabited

/-- An edge in the dependency graph -/
structure Edge where
  /-- Source node id -/
  from_ : String
  /-- Target node id -/
  to : String
  deriving Repr, Inhabited, BEq, Hashable

/-- The complete dependency graph -/
structure Graph where
  /-- All nodes -/
  nodes : Array Node
  /-- All edges -/
  edges : Array Edge
  deriving Repr, Inhabited

/-- Get all node IDs -/
def Graph.nodeIds (g : Graph) : Array String :=
  g.nodes.map (·.id)

/-- Get node by ID -/
def Graph.getNode? (g : Graph) (id : String) : Option Node :=
  g.nodes.find? (·.id == id)

/-- Get outgoing edges from a node -/
def Graph.outEdges (g : Graph) (id : String) : Array Edge :=
  g.edges.filter (·.from_ == id)

/-- Get incoming edges to a node -/
def Graph.inEdges (g : Graph) (id : String) : Array Edge :=
  g.edges.filter (·.to == id)

end Runway.Graph
