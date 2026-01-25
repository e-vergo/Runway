/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Verso.Doc
import Runway.Config
import Runway.Genre

/-!
# Site Structure Types

Types representing the complete Blueprint site structure.
-/

namespace Runway

open Lean (Name Json ToJson FromJson)
open Verso Doc
open Runway.Graph (NodeStatus Graph Node Edge)

/-- Detailed information about a blueprint node for rendering -/
structure NodeInfo where
  /-- The node's unique label -/
  label : String
  /-- Optional display title -/
  title : Option String
  /-- Environment type (theorem, lemma, definition, proposition, corollary, etc.) -/
  envType : String
  /-- Current formalization status -/
  status : NodeStatus
  /-- Pre-rendered HTML for the statement -/
  statementHtml : String
  /-- Pre-rendered HTML for the proof (if any) -/
  proofHtml : Option String
  /-- Pre-rendered Lean code HTML (syntax-highlighted) -/
  codeHtml : Option String := none
  /-- Hover data JSON for Tippy.js tooltips -/
  hoverData : Option String := none
  /-- Associated Lean declaration names -/
  declNames : Array Name
  /-- Labels of nodes this node depends on -/
  uses : Array String
  /-- URL to this node's section in the HTML -/
  url : String
  deriving Inhabited, Repr

instance : ToJson NodeInfo where
  toJson n := .mkObj [
    ("label", .str n.label),
    ("title", match n.title with | some t => .str t | none => .null),
    ("envType", .str n.envType),
    ("status", match n.status with
      | .stated => .str "stated"
      | .proved => .str "proved"
      | .notReady => .str "notReady"
      | .mathLibOk => .str "mathLibOk"),
    ("statementHtml", .str n.statementHtml),
    ("proofHtml", match n.proofHtml with | some p => .str p | none => .null),
    ("codeHtml", match n.codeHtml with | some c => .str c | none => .null),
    ("hoverData", match n.hoverData with | some h => .str h | none => .null),
    ("declNames", .arr (n.declNames.map fun name => .str name.toString)),
    ("uses", .arr (n.uses.map .str)),
    ("url", .str n.url)
  ]

instance : FromJson NodeInfo where
  fromJson? j := do
    let label ← j.getObjValAs? String "label"
    let title := (j.getObjValAs? String "title").toOption
    let envType ← j.getObjValAs? String "envType"
    let statusStr ← j.getObjValAs? String "status"
    let status := match statusStr with
      | "proved" => NodeStatus.proved
      | "notReady" => NodeStatus.notReady
      | "mathLibOk" => NodeStatus.mathLibOk
      | _ => NodeStatus.stated
    let statementHtml ← j.getObjValAs? String "statementHtml"
    let proofHtml := (j.getObjValAs? String "proofHtml").toOption
    let codeHtml := (j.getObjValAs? String "codeHtml").toOption
    let hoverData := (j.getObjValAs? String "hoverData").toOption
    let declNamesJson ← j.getObjValAs? (Array String) "declNames" <|> pure #[]
    let declNames := declNamesJson.map (·.toName)
    let uses ← j.getObjValAs? (Array String) "uses" <|> pure #[]
    let url ← j.getObjValAs? String "url" <|> pure ""
    return { label, title, envType, status, statementHtml, proofHtml, codeHtml, hoverData, declNames, uses, url }

/-- A page in the blueprint site -/
structure SitePage where
  /-- Page title -/
  title : String
  /-- URL path for this page -/
  path : String
  /-- The Verso document content -/
  content : Part Blueprint
  deriving Inhabited

/-- The complete Blueprint site -/
structure BlueprintSite where
  /-- Site configuration -/
  config : Config
  /-- All blueprint nodes with their rendered content -/
  nodes : Array NodeInfo
  /-- The dependency graph -/
  depGraph : Graph
  /-- All pages in the site -/
  pages : Array SitePage
  /-- Pre-rendered SVG of the dependency graph (from Dress) -/
  depGraphSvg : Option String := none
  /-- JSON data for the dependency graph (from Dress) -/
  depGraphJson : Option String := none
  deriving Inhabited

namespace BlueprintSite

/-- Get a node by its label -/
def getNode? (site : BlueprintSite) (label : String) : Option NodeInfo :=
  site.nodes.find? (·.label == label)

/-- Get all nodes with a specific status -/
def nodesByStatus (site : BlueprintSite) (status : NodeStatus) : Array NodeInfo :=
  site.nodes.filter (·.status == status)

/-- Get all nodes of a specific environment type -/
def nodesByEnvType (site : BlueprintSite) (envType : String) : Array NodeInfo :=
  site.nodes.filter (·.envType == envType)

/-- Count of nodes by status -/
structure StatusCounts where
  stated : Nat
  proved : Nat
  notReady : Nat
  mathLibOk : Nat
  deriving Inhabited, Repr

/-- Get counts of nodes by status -/
def statusCounts (site : BlueprintSite) : StatusCounts := Id.run do
  let mut counts : StatusCounts := { stated := 0, proved := 0, notReady := 0, mathLibOk := 0 }
  for node in site.nodes do
    match node.status with
    | .stated => counts := { counts with stated := counts.stated + 1 }
    | .proved => counts := { counts with proved := counts.proved + 1 }
    | .notReady => counts := { counts with notReady := counts.notReady + 1 }
    | .mathLibOk => counts := { counts with mathLibOk := counts.mathLibOk + 1 }
  return counts

/-- Total number of nodes -/
def totalNodes (site : BlueprintSite) : Nat :=
  site.nodes.size

/-- Percentage of nodes that are proved or mathLibOk -/
def completionPercentage (site : BlueprintSite) : Float :=
  if site.nodes.isEmpty then 0.0
  else
    let counts := site.statusCounts
    let completed := counts.proved + counts.mathLibOk
    (completed.toFloat / site.nodes.size.toFloat) * 100.0

end BlueprintSite

/-- Builder for constructing a BlueprintSite -/
structure SiteBuilder where
  /-- Site configuration -/
  config : Config
  /-- Accumulated nodes -/
  nodes : Array NodeInfo := #[]
  /-- The dependency graph -/
  depGraph : Graph := { nodes := #[], edges := #[] }
  /-- Accumulated pages -/
  pages : Array SitePage := #[]
  /-- Pre-rendered SVG of the dependency graph -/
  depGraphSvg : Option String := none
  /-- JSON data for the dependency graph -/
  depGraphJson : Option String := none
  deriving Inhabited

namespace SiteBuilder

/-- Create a new site builder -/
def new (config : Config) : SiteBuilder :=
  { config }

/-- Add a node to the site -/
def addNode (builder : SiteBuilder) (node : NodeInfo) : SiteBuilder :=
  { builder with nodes := builder.nodes.push node }

/-- Set the dependency graph -/
def setDepGraph (builder : SiteBuilder) (graph : Graph) : SiteBuilder :=
  { builder with depGraph := graph }

/-- Set the dependency graph SVG -/
def setDepGraphSvg (builder : SiteBuilder) (svg : Option String) : SiteBuilder :=
  { builder with depGraphSvg := svg }

/-- Set the dependency graph JSON -/
def setDepGraphJson (builder : SiteBuilder) (json : Option String) : SiteBuilder :=
  { builder with depGraphJson := json }

/-- Set both dependency graph SVG and JSON -/
def setDepGraphFiles (builder : SiteBuilder) (svg json : Option String) : SiteBuilder :=
  { builder with depGraphSvg := svg, depGraphJson := json }

/-- Add a page to the site -/
def addPage (builder : SiteBuilder) (page : SitePage) : SiteBuilder :=
  { builder with pages := builder.pages.push page }

/-- Build the final site -/
def build (builder : SiteBuilder) : BlueprintSite :=
  { config := builder.config
    nodes := builder.nodes
    depGraph := builder.depGraph
    pages := builder.pages
    depGraphSvg := builder.depGraphSvg
    depGraphJson := builder.depGraphJson }

end SiteBuilder

end Runway
