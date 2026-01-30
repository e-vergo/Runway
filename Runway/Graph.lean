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

/-- Node status for visualization coloring (6 statuses).
    Mirrors Dress.Graph.Types.NodeStatus for compatibility. -/
inductive NodeStatus where
  | notReady     -- Default + Manual: not ready to formalize (sandy brown)
  | ready        -- Manual: ready to formalize (light sea green)
  | sorry        -- Derived: has sorryAx in proof (dark red)
  | proven       -- Derived: formalized without sorry (light green)
  | fullyProven  -- Auto-computed: this + all ancestors proven (forest green)
  | mathlibReady -- Manual: highest priority, ready for/in mathlib (light blue)
  deriving Repr, Inhabited, BEq, DecidableEq

instance : ToJson NodeStatus where
  toJson
    | .notReady => .str "notReady"
    | .ready => .str "ready"
    | .sorry => .str "sorry"
    | .proven => .str "proven"
    | .fullyProven => .str "fullyProven"
    | .mathlibReady => .str "mathlibReady"

instance : FromJson NodeStatus where
  fromJson? j := do
    let s ← j.getStr?
    match s with
    | "notReady" => return .notReady
    | "ready" => return .ready
    | "sorry" => return .sorry
    | "proven" => return .proven
    | "fullyProven" => return .fullyProven
    | "mathlibReady" => return .mathlibReady
    -- Backwards compatibility
    | "stated" => return .notReady
    | "inMathlib" => return .mathlibReady
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
  /-- Module name containing this node (e.g., "PrimeNumberTheoremAnd.Wiener") -/
  moduleName : String := ""
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

/-! ## JSON Serialization -/

open Lean (Json toJson)

instance : ToJson Node where
  toJson n := Json.mkObj [
    ("id", n.id),
    ("label", n.label),
    ("envType", n.envType),
    ("status", toJson n.status),
    ("url", n.url),
    ("leanDecls", toJson (n.leanDecls.map (·.toString))),
    ("moduleName", n.moduleName)
  ]

instance : ToJson Edge where
  toJson e := Json.mkObj [
    ("from", e.from_),
    ("to", e.to)
  ]

instance : ToJson Graph where
  toJson g := Json.mkObj [
    ("nodes", toJson g.nodes),
    ("edges", toJson g.edges)
  ]

/-- Serialize graph to JSON string for embedding in HTML -/
def Graph.toJsonString (g : Graph) : String :=
  (toJson g).compress

end Runway.Graph
