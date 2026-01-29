/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Verso.Doc
import Runway.Config
import Runway.Genre
import Runway.Dress.Load

/-!
# Site Structure Types

Types representing the complete Blueprint site structure.
-/

namespace Runway

open Lean (Name Json ToJson FromJson)
open Verso Doc
open Runway.Graph (NodeStatus Graph Node Edge)

/-! ## Chapter and Section Structures -/

/-- Convert a title to a URL-safe slug -/
def titleToSlug (title : String) : String :=
  title.toLower
    |>.map (fun c => if c.isAlphanum then c else '-')
    |> String.toList
    |> List.filter (fun c => c.isAlphanum || c == '-')
    |> collapseHyphens
    |> String.ofList
    |> dropTrailingHyphens
    |> dropLeadingHyphens
where
  collapseHyphens : List Char → List Char
    | [] => []
    | [c] => [c]
    | '-' :: '-' :: rest => collapseHyphens ('-' :: rest)
    | c :: rest => c :: collapseHyphens rest
  dropTrailingHyphens (s : String) : String :=
    s.toList.reverse.dropWhile (· == '-') |>.reverse |> String.ofList
  dropLeadingHyphens (s : String) : String :=
    s.toList.dropWhile (· == '-') |> String.ofList

/-- Information about a section within a chapter -/
structure SectionInfo where
  /-- Section number (1, 2, 3, ...) or none for unnumbered sections -/
  number : Option Nat := none
  /-- Section title -/
  title : String
  /-- URL-safe slug (e.g., "definition-and-basic-properties") -/
  slug : String
  /-- Rendered prose HTML content -/
  proseHtml : String := ""
  /-- Node labels in this section (resolved at render time) -/
  nodeLabels : Array String := #[]
  deriving Inhabited, Repr

/-- Information about a chapter -/
structure ChapterInfo where
  /-- Chapter number (1, 2, 3, ...) -/
  number : Nat
  /-- Chapter title -/
  title : String
  /-- URL-safe slug (e.g., "psi-function") -/
  slug : String
  /-- Whether this is an appendix chapter -/
  isAppendix : Bool := false
  /-- Rendered prose HTML content -/
  proseHtml : String := ""
  /-- Sections within this chapter -/
  sections : Array SectionInfo := #[]
  /-- Node labels in this chapter (resolved at render time) -/
  nodeLabels : Array String := #[]
  deriving Inhabited, Repr

instance : ToJson SectionInfo where
  toJson s := .mkObj [
    ("number", match s.number with | some n => .num n | none => .null),
    ("title", .str s.title),
    ("slug", .str s.slug),
    ("proseHtml", .str s.proseHtml),
    ("nodeLabels", .arr (s.nodeLabels.map .str))
  ]

instance : ToJson ChapterInfo where
  toJson c := .mkObj [
    ("number", .num c.number),
    ("title", .str c.title),
    ("slug", .str c.slug),
    ("isAppendix", .bool c.isAppendix),
    ("proseHtml", .str c.proseHtml),
    ("sections", .arr (c.sections.map ToJson.toJson)),
    ("nodeLabels", .arr (c.nodeLabels.map .str))
  ]

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
  /-- Pre-rendered Lean signature HTML (syntax-highlighted) -/
  signatureHtml : Option String := none
  /-- Pre-rendered Lean proof body HTML (syntax-highlighted) -/
  proofBodyHtml : Option String := none
  /-- Hover data JSON for Tippy.js tooltips -/
  hoverData : Option String := none
  /-- Associated Lean declaration names -/
  declNames : Array Name
  /-- Labels of nodes this node depends on -/
  uses : Array String
  /-- URL to this node's section in the HTML -/
  url : String
  /-- Display number in chapter.section.item format (e.g., "4.1.1") -/
  displayNumber : Option String := none
  /-- Optional custom display name for rendering -/
  displayName : Option String := none
  /-- Whether this is a key declaration for dashboard highlighting -/
  keyDeclaration : Bool := false
  /-- Optional message annotation -/
  message : Option String := none
  /-- Whether this is a priority item for dashboard display -/
  priorityItem : Bool := false
  /-- Description of what's blocking this node -/
  blocked : Option String := none
  /-- Description of potential issues -/
  potentialIssue : Option String := none
  /-- Description of technical debt -/
  technicalDebt : Option String := none
  /-- Miscellaneous notes -/
  misc : Option String := none
  deriving Inhabited, Repr

instance : ToJson NodeInfo where
  toJson n := .mkObj [
    ("label", .str n.label),
    ("title", match n.title with | some t => .str t | none => .null),
    ("envType", .str n.envType),
    ("status", match n.status with
      | .notReady => .str "notReady"
      | .stated => .str "stated"
      | .ready => .str "ready"
      | .sorry => .str "sorry"
      | .proven => .str "proven"
      | .fullyProven => .str "fullyProven"
      | .mathlibReady => .str "mathlibReady"
      | .inMathlib => .str "inMathlib"),
    ("statementHtml", .str n.statementHtml),
    ("proofHtml", match n.proofHtml with | some p => .str p | none => .null),
    ("signatureHtml", match n.signatureHtml with | some c => .str c | none => .null),
    ("proofBodyHtml", match n.proofBodyHtml with | some c => .str c | none => .null),
    ("hoverData", match n.hoverData with | some h => .str h | none => .null),
    ("declNames", .arr (n.declNames.map fun name => .str name.toString)),
    ("uses", .arr (n.uses.map .str)),
    ("url", .str n.url),
    ("displayNumber", match n.displayNumber with | some d => .str d | none => .null),
    ("displayName", match n.displayName with | some d => .str d | none => .null),
    ("keyDeclaration", .bool n.keyDeclaration),
    ("message", match n.message with | some m => .str m | none => .null),
    ("priorityItem", .bool n.priorityItem),
    ("blocked", match n.blocked with | some b => .str b | none => .null),
    ("potentialIssue", match n.potentialIssue with | some p => .str p | none => .null),
    ("technicalDebt", match n.technicalDebt with | some t => .str t | none => .null),
    ("misc", match n.misc with | some m => .str m | none => .null)
  ]

instance : FromJson NodeInfo where
  fromJson? j := do
    let label ← j.getObjValAs? String "label"
    let title := (j.getObjValAs? String "title").toOption
    let envType ← j.getObjValAs? String "envType"
    let statusStr ← j.getObjValAs? String "status"
    let status := match statusStr with
      | "notReady" => NodeStatus.notReady
      | "stated" => NodeStatus.stated
      | "ready" => NodeStatus.ready
      | "sorry" => NodeStatus.sorry
      | "proven" => NodeStatus.proven
      | "fullyProven" => NodeStatus.fullyProven
      | "mathlibReady" => NodeStatus.mathlibReady
      | "inMathlib" => NodeStatus.inMathlib
      | _ => NodeStatus.stated
    let statementHtml ← j.getObjValAs? String "statementHtml"
    let proofHtml := (j.getObjValAs? String "proofHtml").toOption
    let signatureHtml := (j.getObjValAs? String "signatureHtml").toOption
    let proofBodyHtml := (j.getObjValAs? String "proofBodyHtml").toOption
    let hoverData := (j.getObjValAs? String "hoverData").toOption
    let declNamesJson ← j.getObjValAs? (Array String) "declNames" <|> pure #[]
    let declNames := declNamesJson.map (·.toName)
    let uses ← j.getObjValAs? (Array String) "uses" <|> pure #[]
    let url ← j.getObjValAs? String "url" <|> pure ""
    let displayNumber := (j.getObjValAs? String "displayNumber").toOption
    let displayName := (j.getObjValAs? String "displayName").toOption
    let keyDeclaration := (j.getObjValAs? Bool "keyDeclaration").toOption.getD false
    let message := (j.getObjValAs? String "message").toOption
    let priorityItem := (j.getObjValAs? Bool "priorityItem").toOption.getD false
    let blocked := (j.getObjValAs? String "blocked").toOption
    let potentialIssue := (j.getObjValAs? String "potentialIssue").toOption
    let technicalDebt := (j.getObjValAs? String "technicalDebt").toOption
    let misc := (j.getObjValAs? String "misc").toOption
    return { label, title, envType, status, statementHtml, proofHtml, signatureHtml, proofBodyHtml, hoverData, declNames, uses, url, displayNumber, displayName, keyDeclaration, message, priorityItem, blocked, potentialIssue, technicalDebt, misc }

/-- A page in the blueprint site -/
structure SitePage where
  /-- Page title -/
  title : String
  /-- URL path for this page -/
  path : String
  /-- The Verso document content -/
  content : Part Blueprint
  deriving Inhabited

/-! ## Status Counts -/

/-- Count of nodes by status -/
structure StatusCounts where
  notReady : Nat := 0
  stated : Nat := 0
  ready : Nat := 0
  hasSorry : Nat := 0  -- Named hasSorry because `sorry` is a keyword
  proven : Nat := 0
  fullyProven : Nat := 0
  mathlibReady : Nat := 0
  inMathlib : Nat := 0
  total : Nat := 0
  deriving Inhabited, Repr

instance : ToJson StatusCounts where
  toJson sc := .mkObj [
    ("notReady", .num sc.notReady),
    ("stated", .num sc.stated),
    ("ready", .num sc.ready),
    ("hasSorry", .num sc.hasSorry),
    ("proven", .num sc.proven),
    ("fullyProven", .num sc.fullyProven),
    ("mathlibReady", .num sc.mathlibReady),
    ("inMathlib", .num sc.inMathlib),
    ("total", .num sc.total)
  ]

instance : FromJson StatusCounts where
  fromJson? j := do
    let notReady ← j.getObjValAs? Nat "notReady" <|> pure 0
    let stated ← j.getObjValAs? Nat "stated" <|> pure 0
    let ready ← j.getObjValAs? Nat "ready" <|> pure 0
    let hasSorry ← j.getObjValAs? Nat "hasSorry" <|> pure 0
    let proven ← j.getObjValAs? Nat "proven" <|> pure 0
    let fullyProven ← j.getObjValAs? Nat "fullyProven" <|> pure 0
    let mathlibReady ← j.getObjValAs? Nat "mathlibReady" <|> pure 0
    let inMathlib ← j.getObjValAs? Nat "inMathlib" <|> pure 0
    let total ← j.getObjValAs? Nat "total" <|> pure 0
    return { notReady, stated, ready, hasSorry, proven, fullyProven, mathlibReady, inMathlib, total }

/-! ## Blueprint Site -/

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
  /-- Chapters extracted from blueprint.tex (for multi-page navigation) -/
  chapters : Array ChapterInfo := #[]
  /-- Precomputed status counts from manifest -/
  precomputedStats : Option StatusCounts := none
  /-- Graph validation check results from manifest -/
  checks : Option Dress.CheckResults := none
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

/-- Compute counts of nodes by status from nodes array -/
def computeStatusCounts (site : BlueprintSite) : StatusCounts := Id.run do
  let mut counts : StatusCounts := {}
  for node in site.nodes do
    match node.status with
    | .notReady => counts := { counts with notReady := counts.notReady + 1 }
    | .stated => counts := { counts with stated := counts.stated + 1 }
    | .ready => counts := { counts with ready := counts.ready + 1 }
    | .sorry => counts := { counts with hasSorry := counts.hasSorry + 1 }
    | .proven => counts := { counts with proven := counts.proven + 1 }
    | .fullyProven => counts := { counts with fullyProven := counts.fullyProven + 1 }
    | .mathlibReady => counts := { counts with mathlibReady := counts.mathlibReady + 1 }
    | .inMathlib => counts := { counts with inMathlib := counts.inMathlib + 1 }
  return { counts with total := site.nodes.size }

/-- Get counts of nodes by status (uses precomputed if available, otherwise computes) -/
def statusCounts (site : BlueprintSite) : StatusCounts :=
  site.precomputedStats.getD site.computeStatusCounts

/-- Alias for statusCounts that explicitly prefers precomputed stats -/
def getStatusCounts (site : BlueprintSite) : StatusCounts :=
  site.statusCounts

/-- Total number of nodes -/
def totalNodes (site : BlueprintSite) : Nat :=
  site.nodes.size

/-- Percentage of nodes that are proven, fullyProven, mathlibReady, or inMathlib -/
def completionPercentage (site : BlueprintSite) : Float :=
  if site.nodes.isEmpty then 0.0
  else
    let counts := site.statusCounts
    let completed := counts.proven + counts.fullyProven + counts.mathlibReady + counts.inMathlib
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
  /-- Chapters extracted from blueprint.tex -/
  chapters : Array ChapterInfo := #[]
  /-- Precomputed status counts from manifest -/
  precomputedStats : Option StatusCounts := none
  /-- Graph validation check results from manifest -/
  checks : Option Dress.CheckResults := none
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

/-- Set chapters -/
def setChapters (builder : SiteBuilder) (chapters : Array ChapterInfo) : SiteBuilder :=
  { builder with chapters := chapters }

/-- Add a chapter to the site -/
def addChapter (builder : SiteBuilder) (chapter : ChapterInfo) : SiteBuilder :=
  { builder with chapters := builder.chapters.push chapter }

/-- Set precomputed status counts -/
def setPrecomputedStats (builder : SiteBuilder) (stats : Option StatusCounts) : SiteBuilder :=
  { builder with precomputedStats := stats }

/-- Set graph validation check results -/
def setChecks (builder : SiteBuilder) (checks : Option Dress.CheckResults) : SiteBuilder :=
  { builder with checks := checks }

/-- Build the final site -/
def build (builder : SiteBuilder) : BlueprintSite :=
  { config := builder.config
    nodes := builder.nodes
    depGraph := builder.depGraph
    pages := builder.pages
    depGraphSvg := builder.depGraphSvg
    depGraphJson := builder.depGraphJson
    chapters := builder.chapters
    precomputedStats := builder.precomputedStats
    checks := builder.checks }

end SiteBuilder

end Runway
