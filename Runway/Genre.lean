/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Std.Data.HashMap
import Std.Data.HashSet
import Verso.Doc
import Runway.Config
import Runway.Graph

/-!
# Blueprint Genre for Verso

Defines the Verso Genre for Blueprint documents, enabling structured
document creation with blueprint-specific extensions.
-/

namespace Runway

open Lean (Name Json ToJson FromJson)
open Std (HashMap HashSet)
open Verso Doc
open Runway.Graph (NodeStatus Graph Node)

/-! ## Part Metadata -/

/-- Metadata for document parts (chapters, sections, etc.) -/
structure Blueprint.Meta where
  /-- Optional title override -/
  title : Option String := none
  /-- HTML ID for this part -/
  htmlId : Option String := none
  /-- Associated node status (if this part represents a blueprint node) -/
  status : Option NodeStatus := none
  /-- Lean declaration names associated with this part -/
  declNames : Array Name := #[]
  /-- Whether to show this part in navigation -/
  showInNav : Bool := true
  deriving Inhabited, Repr

instance : BEq Blueprint.Meta where
  beq m1 m2 :=
    m1.title == m2.title &&
    m1.htmlId == m2.htmlId &&
    m1.declNames == m2.declNames &&
    m1.showInNav == m2.showInNav

instance : ToJson Blueprint.Meta where
  toJson m := .mkObj [
    ("title", match m.title with | some t => .str t | none => .null),
    ("htmlId", match m.htmlId with | some h => .str h | none => .null),
    ("declNames", .arr (m.declNames.map fun n => .str n.toString)),
    ("showInNav", .bool m.showInNav)
  ]

instance : FromJson Blueprint.Meta where
  fromJson? j := do
    let title := (j.getObjValAs? String "title").toOption
    let htmlId := (j.getObjValAs? String "htmlId").toOption
    let declNamesJson ← j.getObjValAs? (Array String) "declNames" <|> pure #[]
    let declNames := declNamesJson.map (·.toName)
    let showInNav ← j.getObjValAs? Bool "showInNav" <|> pure true
    return { title, htmlId, status := none, declNames, showInNav }

/-! ## Block Extensions -/

/-- Additional block-level elements for Blueprint documents -/
inductive Blueprint.BlockExt where
  /-- Embed a blueprint node by its label -/
  | node (label : String)
  /-- Embed just the proof of a blueprint node -/
  | proof (label : String)
  /-- A highlighted code block -/
  | code (language : String) (content : String)
  /-- A wrapper div with CSS classes -/
  | htmlDiv (classes : String)
  /-- The dependency graph visualization -/
  | depGraph
  /-- A theorem/lemma/definition environment -/
  | theoremEnv (envType : String) (label : Option String)
  deriving Inhabited, Repr, BEq

instance : ToJson Blueprint.BlockExt where
  toJson
    | .node label => .mkObj [("node", .str label)]
    | .proof label => .mkObj [("proof", .str label)]
    | .code lang content => .mkObj [("code", .mkObj [("language", .str lang), ("content", .str content)])]
    | .htmlDiv classes => .mkObj [("htmlDiv", .str classes)]
    | .depGraph => .mkObj [("depGraph", .null)]
    | .theoremEnv envType label => .mkObj [("theoremEnv", .mkObj [
        ("envType", .str envType),
        ("label", match label with | some l => .str l | none => .null)
      ])]

instance : FromJson Blueprint.BlockExt where
  fromJson? j := do
    if let some v := j.getObjVal? "node" |>.toOption then
      return .node (← FromJson.fromJson? v)
    else if let some v := j.getObjVal? "proof" |>.toOption then
      return .proof (← FromJson.fromJson? v)
    else if let some v := j.getObjVal? "code" |>.toOption then
      let lang ← v.getObjValAs? String "language"
      let content ← v.getObjValAs? String "content"
      return .code lang content
    else if let some v := j.getObjVal? "htmlDiv" |>.toOption then
      return .htmlDiv (← FromJson.fromJson? v)
    else if j.getObjVal? "depGraph" |>.toOption |>.isSome then
      return .depGraph
    else if let some v := j.getObjVal? "theoremEnv" |>.toOption then
      let envType ← v.getObjValAs? String "envType"
      let label := (v.getObjValAs? String "label").toOption
      return .theoremEnv envType label
    else
      throw "Unknown Blueprint.BlockExt"

/-! ## Inline Extensions -/

/-- Additional inline-level elements for Blueprint documents -/
inductive Blueprint.InlineExt where
  /-- Reference to a Lean declaration (renders as link to docs) -/
  | leanRef (name : Name)
  /-- Inline LaTeX math -/
  | math (latex : String)
  /-- Reference to a blueprint node (renders as link) -/
  | nodeRef (label : String)
  /-- An HTML span with CSS classes -/
  | htmlSpan (classes : String)
  deriving Inhabited, Repr, BEq

instance : ToJson Blueprint.InlineExt where
  toJson
    | .leanRef name => .mkObj [("leanRef", .str name.toString)]
    | .math latex => .mkObj [("math", .str latex)]
    | .nodeRef label => .mkObj [("nodeRef", .str label)]
    | .htmlSpan classes => .mkObj [("htmlSpan", .str classes)]

instance : FromJson Blueprint.InlineExt where
  fromJson? j := do
    if let some v := j.getObjVal? "leanRef" |>.toOption then
      return .leanRef (← FromJson.fromJson? (α := String) v).toName
    else if let some v := j.getObjVal? "math" |>.toOption then
      return .math (← FromJson.fromJson? v)
    else if let some v := j.getObjVal? "nodeRef" |>.toOption then
      return .nodeRef (← FromJson.fromJson? v)
    else if let some v := j.getObjVal? "htmlSpan" |>.toOption then
      return .htmlSpan (← FromJson.fromJson? v)
    else
      throw "Unknown Blueprint.InlineExt"

/-! ## Traversal Context and State -/

/-- Read-only context available during document traversal -/
structure Blueprint.Context where
  /-- Site configuration -/
  config : Config
  /-- The dependency graph from Dress -/
  depGraph : Graph
  deriving Inhabited

/-- Mutable state during document traversal -/
structure Blueprint.State where
  /-- Nodes loaded from Dress artifacts, keyed by label -/
  nodes : HashMap String Node := {}
  /-- Set of node labels that have been referenced -/
  usedRefs : HashSet String := {}
  /-- Errors encountered during traversal -/
  errors : HashSet String := {}
  /-- Collected HTML IDs for duplicate detection -/
  usedIds : HashSet String := {}
  deriving Inhabited

private def hashSetEq (s1 s2 : HashSet String) : Bool :=
  s1.size == s2.size &&
  s1.fold (fun acc k => acc && s2.contains k) true

private def hashMapEq (m1 m2 : HashMap String Node) : Bool :=
  m1.size == m2.size &&
  m1.fold (fun acc k v =>
    acc && match m2.get? k with
      | some v2 => v.id == v2.id && v.label == v2.label && v.status == v2.status
      | none => false) true

instance : BEq Blueprint.State where
  beq s1 s2 :=
    hashMapEq s1.nodes s2.nodes &&
    hashSetEq s1.usedRefs s2.usedRefs &&
    hashSetEq s1.errors s2.errors &&
    hashSetEq s1.usedIds s2.usedIds

/-! ## The Genre Definition -/

/-- The Blueprint genre for Verso documents -/
def Blueprint : Genre where
  PartMetadata := Blueprint.Meta
  Block := Blueprint.BlockExt
  Inline := Blueprint.InlineExt
  TraverseContext := Blueprint.Context
  TraverseState := Blueprint.State

-- Type aliases for convenience
abbrev BlueprintPart := Part Blueprint
abbrev BlueprintBlock := Block Blueprint
abbrev BlueprintInline := Inline Blueprint

-- Instances needed by Verso
instance : Repr Blueprint.PartMetadata := inferInstanceAs (Repr Blueprint.Meta)
instance : Repr Blueprint.Block := inferInstanceAs (Repr Blueprint.BlockExt)
instance : Repr Blueprint.Inline := inferInstanceAs (Repr Blueprint.InlineExt)
instance : BEq Blueprint.PartMetadata := inferInstanceAs (BEq Blueprint.Meta)
instance : BEq Blueprint.Block := inferInstanceAs (BEq Blueprint.BlockExt)
instance : BEq Blueprint.Inline := inferInstanceAs (BEq Blueprint.InlineExt)
instance : ToJson Blueprint.PartMetadata := inferInstanceAs (ToJson Blueprint.Meta)
instance : ToJson Blueprint.Block := inferInstanceAs (ToJson Blueprint.BlockExt)
instance : ToJson Blueprint.Inline := inferInstanceAs (ToJson Blueprint.InlineExt)
instance : FromJson Blueprint.PartMetadata := inferInstanceAs (FromJson Blueprint.Meta)
instance : FromJson Blueprint.Block := inferInstanceAs (FromJson Blueprint.BlockExt)
instance : FromJson Blueprint.Inline := inferInstanceAs (FromJson Blueprint.InlineExt)

end Runway
