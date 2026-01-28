/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Runway
import Runway.Paper

/-!
# Runway CLI

Command-line interface for generating static sites from Dress artifacts.

## Usage

```
runway [options] [config.json]

Options:
  --build-dir <path>   Lake build directory (default: .lake/build)
  --output <path>      Output directory (default: .lake/build/runway)
  --help, -h           Show this help message
  --version, -v        Show version information

Commands:
  build                Generate HTML from Dress artifacts (default)
  serve                Start local HTTP server for preview
  check                Verify Lean declarations exist
```
-/

namespace Runway.CLI

open System (FilePath)
open Std (HashMap HashSet)
open Runway (loadDepGraph loadDeclArtifacts DeclArtifact)
open Runway.Dress (loadEnhancedManifest EnhancedManifest)
open Runway.Latex (parseFile extractChapters extractSections extractModuleRefs extractNodeRefs toHtml)

/-- CLI configuration parsed from command-line arguments -/
structure CLIConfig where
  /-- Path to the runway.json config file -/
  configPath : FilePath := "runway.json"
  /-- Lake build directory containing Dress artifacts -/
  buildDir : FilePath := ".lake/build"
  /-- Output directory for generated site -/
  outputDir : Option FilePath := none
  /-- Show help message -/
  showHelp : Bool := false
  /-- Show version -/
  showVersion : Bool := false
  /-- Command to execute -/
  command : String := "build"
  /-- Path to paper.tex (optional, overrides config) -/
  paperTexPath : Option FilePath := none
  deriving Repr, Inhabited

/-- Parse command-line arguments into CLIConfig -/
def parseArgs (args : List String) : Except String CLIConfig := do
  let mut config : CLIConfig := {}
  let mut remaining := args
  let mut positionalArgs : List String := []

  while !remaining.isEmpty do
    match remaining with
    | [] => break
    | "--help" :: rest | "-h" :: rest =>
      config := { config with showHelp := true }
      remaining := rest
    | "--version" :: rest | "-v" :: rest =>
      config := { config with showVersion := true }
      remaining := rest
    | "--build-dir" :: path :: rest =>
      config := { config with buildDir := path }
      remaining := rest
    | "--output" :: path :: rest =>
      config := { config with outputDir := some path }
      remaining := rest
    | "--build-dir" :: [] | "--output" :: [] =>
      throw "Missing argument for option"
    | arg :: rest =>
      if arg.startsWith "--" then
        throw s!"Unknown option: {arg}"
      else if arg.startsWith "-" then
        throw s!"Unknown option: {arg}"
      else
        positionalArgs := positionalArgs ++ [arg]
      remaining := rest

  -- Parse positional arguments: [command] [config.json]
  match positionalArgs with
  | [] => pure ()
  | [cmd] =>
    if cmd == "build" || cmd == "serve" || cmd == "check" || cmd == "paper" then
      config := { config with command := cmd }
    else if cmd.endsWith ".json" then
      config := { config with configPath := cmd }
    else
      config := { config with command := cmd }
  | [cmd, configPath] =>
    config := { config with command := cmd, configPath := configPath }
  | _ => throw "Too many positional arguments"

  return config

/-- Load configuration from JSON file -/
def loadConfig (path : FilePath) : IO Config := do
  IO.println s!"[DEBUG] loadConfig: checking if path exists: {path}"
  (← IO.getStdout).flush
  if ← path.pathExists then
    IO.println "[DEBUG] loadConfig: path exists, reading file..."
    (← IO.getStdout).flush
    let content ← IO.FS.readFile path
    IO.println s!"[DEBUG] loadConfig: file read, {content.length} bytes"
    (← IO.getStdout).flush
    IO.println "[DEBUG] loadConfig: parsing JSON..."
    (← IO.getStdout).flush
    match Lean.Json.parse content >>= Lean.FromJson.fromJson? with
    | .ok config =>
      IO.println "[DEBUG] loadConfig: JSON parsed successfully"
      (← IO.getStdout).flush
      return config
    | .error e => throw <| IO.userError s!"Failed to parse config: {e}"
  else
    throw <| IO.userError s!"ERROR: Config file not found at {path}. A config file with 'assetsDir' is required."

/-- Load the dependency graph SVG file if it exists -/
def loadDepGraphSvg (dressedDir : FilePath) : IO (Option String) := do
  let svgPath := dressedDir / "dep-graph.svg"
  if ← svgPath.pathExists then
    return some (← IO.FS.readFile svgPath)
  else
    return none

/-- Load the dependency graph JSON file if it exists -/
def loadDepGraphJson (dressedDir : FilePath) : IO (Option String) := do
  let jsonPath := dressedDir / "dep-graph.json"
  if ← jsonPath.pathExists then
    return some (← IO.FS.readFile jsonPath)
  else
    return none

/-- Load and parse the blueprint.tex file to extract chapters -/
def loadBlueprintChapters (config : Config) (allNodes : Array NodeInfo) : IO (Array ChapterInfo) := do
  IO.println "[DEBUG] loadBlueprintChapters: Starting..."
  match config.blueprintTexPath with
  | none =>
    IO.println "[DEBUG] No blueprintTexPath configured, returning empty"
    return #[]
  | some texPath =>
    let path : FilePath := texPath
    IO.println s!"[DEBUG] blueprintTexPath = {texPath}"
    if !(← path.pathExists) then
      IO.eprintln s!"Warning: Blueprint tex file not found at {texPath}"
      return #[]

    IO.println s!"  - Loading blueprint structure from {texPath}"
    IO.println "[DEBUG] Calling parseFile..."
    let (doc, errors) ← parseFile path
    IO.println s!"[DEBUG] parseFile complete, {errors.size} errors"

    for err in errors do
      IO.eprintln s!"    LaTeX parse warning: {err}"

    -- Extract chapters from document
    IO.println "[DEBUG] Extracting document body..."
    let docBody := match doc.root with
      | .document _ body => body
      | other => #[other]
    IO.println s!"[DEBUG] docBody has {docBody.size} elements"

    IO.println "[DEBUG] Extracting chapters..."
    let chapterExtracts := extractChapters docBody
    IO.println s!"[DEBUG] Found {chapterExtracts.size} chapter extracts"
    IO.println s!"  - Found {chapterExtracts.size} chapters"

    -- Build a map of module name -> nodes for quick lookup
    IO.println s!"[DEBUG] Building moduleToNodes map from {allNodes.size} nodes..."
    let moduleToNodes : HashMap Lean.Name (Array NodeInfo) := Id.run do
      let mut m : HashMap Lean.Name (Array NodeInfo) := {}
      for node in allNodes do
        for declName in node.declNames do
          -- Extract module from declaration name (e.g., Crystallographic.Psi.Basic.psi -> Crystallographic.Psi.Basic)
          let parts := declName.components
          if parts.length > 1 then
            let moduleParts := parts.dropLast
            let moduleName := moduleParts.foldl (fun acc p => acc ++ p) Lean.Name.anonymous
            m := m.insert moduleName (m.getD moduleName #[] |>.push node)
      return m
    IO.println s!"[DEBUG] moduleToNodes map built: {moduleToNodes.size} modules"

    -- Build a map of node label -> NodeInfo for quick lookup
    IO.println "[DEBUG] Building labelToNode map..."
    let labelToNode : HashMap String NodeInfo := Id.run do
      let mut m : HashMap String NodeInfo := {}
      for node in allNodes do
        m := m.insert node.label node
      return m
    IO.println s!"[DEBUG] labelToNode map built: {labelToNode.size} labels"

    -- Convert chapter extracts to ChapterInfo
    IO.println s!"[DEBUG] Converting {chapterExtracts.size} chapter extracts to ChapterInfo..."
    let mut chapters : Array ChapterInfo := #[]
    let mut chapterIdx := 0

    for ce in chapterExtracts do
      chapterIdx := chapterIdx + 1
      IO.println s!"[DEBUG]   Processing chapter {chapterIdx}/{chapterExtracts.size}: {ce.title}"
      -- Extract module and node references from chapter body
      let moduleRefs := extractModuleRefs ce.body
      let nodeRefs := extractNodeRefs ce.body

      -- Collect nodes for this chapter
      let mut chapterNodes : Array NodeInfo := #[]

      -- Add nodes from module references
      for modName in moduleRefs do
        match moduleToNodes.get? modName with
        | some nodes => chapterNodes := chapterNodes ++ nodes
        | none => pure ()

      -- Add nodes from direct node references
      for nodeLabel in nodeRefs do
        -- Normalize label: colons become hyphens during artifact loading
        let normalizedLabel := nodeLabel.replace ":" "-"
        match labelToNode.get? normalizedLabel with
        | some node =>
          -- Avoid duplicates
          if !chapterNodes.any (·.label == node.label) then
            chapterNodes := chapterNodes.push node
        | none => pure ()

      -- Extract sections
      IO.println s!"[DEBUG]     Extracting sections from chapter..."
      let sectionExtracts := extractSections ce.body
      IO.println s!"[DEBUG]     Found {sectionExtracts.size} sections"
      let mut sectionInfos : Array SectionInfo := #[]

      for se in sectionExtracts do
        IO.println s!"[DEBUG]       Processing section: {se.title}"
        -- Collect nodes for this section
        let sectionModuleRefs := extractModuleRefs se.body
        let sectionNodeRefs := extractNodeRefs se.body

        let mut sectionNodes : Array NodeInfo := #[]
        for modName in sectionModuleRefs do
          match moduleToNodes.get? modName with
          | some nodes => sectionNodes := sectionNodes ++ nodes
          | none => pure ()

        for nodeLabel in sectionNodeRefs do
          -- Normalize label: colons become hyphens during artifact loading
          let normalizedLabel := nodeLabel.replace ":" "-"
          match labelToNode.get? normalizedLabel with
          | some node =>
            if !sectionNodes.any (·.label == node.label) then
              sectionNodes := sectionNodes.push node
          | none => pure ()

        -- Convert section body to HTML (excluding \inputleanmodule/\inputleannode which become placeholders)
        IO.println s!"[DEBUG]       Converting section body to HTML..."
        let sectionHtmlResult := toHtml se.body
        IO.println s!"[DEBUG]       Section HTML conversion complete"

        sectionInfos := sectionInfos.push {
          number := se.number
          title := se.title
          slug := titleToSlug se.title
          nodeLabels := sectionNodes.map (·.label)
          proseHtml := sectionHtmlResult.html
        }

      -- Convert chapter body to HTML
      IO.println s!"[DEBUG]     Converting chapter body to HTML..."
      let chapterHtmlResult := toHtml ce.body
      IO.println s!"[DEBUG]     Chapter HTML conversion complete"

      chapters := chapters.push {
        number := ce.number
        title := ce.title
        slug := titleToSlug ce.title
        isAppendix := ce.isAppendix
        nodeLabels := chapterNodes.map (·.label)
        proseHtml := chapterHtmlResult.html
        sections := sectionInfos
      }

    IO.println s!"[DEBUG] loadBlueprintChapters: Complete, returning {chapters.size} chapters"
    return chapters

/-- Assign display numbers to nodes based on their position in chapters/sections.
    Format: ChapterNum.SectionNum.ItemNum (e.g., "4.1.1", "4.1.2", "4.2.1")
    If a node appears in chapter prose (not in a section), uses ChapterNum.ItemNum.
    Nodes not found in any chapter get no display number. -/
def assignDisplayNumbers (nodes : Array NodeInfo) (chapters : Array ChapterInfo) : Array NodeInfo := Id.run do
  -- Build a mutable map from label to position info
  let mut labelToNumber : HashMap String String := {}

  for chapter in chapters do
    let chapterNum := chapter.number
    -- Track item count for chapter-level nodes (not in any section)
    let mut chapterItemCount : Nat := 0

    -- Process sections first
    for sec in chapter.sections do
      match sec.number with
      | some secNum =>
        -- Numbered section: items are ChapterNum.SecNum.ItemNum
        let mut sectionItemCount : Nat := 0
        for nodeLabel in sec.nodeLabels do
          sectionItemCount := sectionItemCount + 1
          let displayNum := s!"{chapterNum}.{secNum}.{sectionItemCount}"
          labelToNumber := labelToNumber.insert nodeLabel displayNum
      | none =>
        -- Unnumbered section: skip numbering its items
        pure ()

    -- Process chapter-level nodes (in chapter.nodeLabels but not in any section)
    -- These are nodes referenced at chapter level, not within sections
    let sectionLabels : Std.HashSet String := Id.run do
      let mut s : Std.HashSet String := {}
      for sec in chapter.sections do
        for label in sec.nodeLabels do
          s := s.insert label
      return s

    for nodeLabel in chapter.nodeLabels do
      if !sectionLabels.contains nodeLabel && !labelToNumber.contains nodeLabel then
        chapterItemCount := chapterItemCount + 1
        let displayNum := s!"{chapterNum}.{chapterItemCount}"
        labelToNumber := labelToNumber.insert nodeLabel displayNum

  -- Apply display numbers to nodes
  return nodes.map fun node =>
    match labelToNumber.get? node.label with
    | some num => { node with displayNumber := some num }
    | none => node

/-- Parse StatusCounts from JSON -/
def parseStatusCounts (json : Lean.Json) : Option StatusCounts :=
  let getNat (key : String) : Nat := (json.getObjValAs? Nat key).toOption.getD 0
  some {
    notReady := getNat "notReady"
    stated := getNat "stated"
    ready := getNat "ready"
    hasSorry := getNat "hasSorry"
    proven := getNat "proven"
    fullyProven := getNat "fullyProven"
    mathlibReady := getNat "mathlibReady"
    inMathlib := getNat "inMathlib"
    total := getNat "total"
  }

/-- Build a BlueprintSite from Dress artifacts -/
def buildSiteFromArtifacts (config : Config) (dressedDir : FilePath) : IO BlueprintSite := do
  IO.println "[DEBUG] buildSiteFromArtifacts: Starting..."
  IO.println s!"[DEBUG] dressedDir = {dressedDir}"

  -- Load the enhanced manifest (contains stats and node metadata)
  IO.println "[DEBUG] Loading enhanced manifest..."
  let manifest ← loadEnhancedManifest dressedDir
  IO.println "[DEBUG] Enhanced manifest loaded"
  if manifest.hasStats then
    IO.println s!"  - Loaded enhanced manifest with precomputed stats"

  -- Parse precomputed stats from manifest if available
  IO.println "[DEBUG] Parsing precomputed stats..."
  let precomputedStats := manifest.stats.bind parseStatusCounts
  IO.println "[DEBUG] Precomputed stats parsed"

  -- Load the dependency graph
  IO.println "[DEBUG] Loading dependency graph..."
  let depGraph ← loadDepGraph dressedDir
  IO.println s!"[DEBUG] Dependency graph loaded: {depGraph.nodes.size} nodes, {depGraph.edges.size} edges"

  -- Load SVG for the graph (may be empty if not generated by Dress)
  IO.println "[DEBUG] Loading dep graph SVG..."
  let depGraphSvg ← loadDepGraphSvg dressedDir
  IO.println s!"[DEBUG] SVG loaded: {if depGraphSvg.isSome then "yes" else "no"}"

  -- Generate JSON from the graph we built (don't read from potentially empty file)
  IO.println "[DEBUG] Generating dep graph JSON string..."
  let depGraphJson := some depGraph.toJsonString
  IO.println "[DEBUG] JSON string generated"

  -- Load decl.tex artifacts (contains statement/proof HTML)
  IO.println "[DEBUG] Loading declaration artifacts..."
  let declArtifacts ← loadDeclArtifacts dressedDir
  IO.println s!"[DEBUG] Declaration artifacts loaded: {declArtifacts.size}"
  IO.println s!"  - Loaded {declArtifacts.size} declaration artifacts from .tex files"

  -- Convert graph nodes to NodeInfo, populating HTML from artifacts
  IO.println s!"[DEBUG] Converting {depGraph.nodes.size} graph nodes to NodeInfo..."
  let mut nodes : Array NodeInfo := #[]
  let mut nodeCount := 0
  for node in depGraph.nodes do
    nodeCount := nodeCount + 1
    if nodeCount % 10 == 0 then
      IO.println s!"[DEBUG]   Processed {nodeCount}/{depGraph.nodes.size} nodes..."
    -- Normalize node.id: artifacts are keyed with hyphens (colon -> hyphen)
    let normalizedId := node.id.replace ":" "-"
    -- Look up artifact by normalized id
    let artifact := declArtifacts.get? normalizedId

    -- Extract Lean signature and proof body HTML separately for right column
    let signatureHtml := match artifact with
      | some art => art.leanSignatureHtml.filter (·.isEmpty == false)
      | none => none
    let proofBodyHtml := match artifact with
      | some art => art.leanProofBodyHtml.filter (·.isEmpty == false)
      | none => none

    -- Use hover data from artifact (already loaded during initial traversal)
    -- The hoverData comes from \leanhoverdata{base64} in decl.tex, decoded by parseDeclTex
    let hoverData := artifact.bind (·.hoverData)

    -- Get node metadata from manifest (use original node.id for lookup)
    let keyTheorem := manifest.isKeyTheorem node.id
    let message := manifest.getMessage node.id
    let priorityItem := manifest.getPriorityItem node.id
    let blocked := manifest.getBlocked node.id
    let potentialIssue := manifest.getPotentialIssue node.id
    let technicalDebt := manifest.getTechnicalDebt node.id
    let misc := manifest.getMisc node.id

    -- Determine displayName:
    -- node.label from Dress contains displayName (if set) or full Lean name
    -- If node.label looks like a qualified Lean name (contains "."), derive short name from leanDecls
    -- Otherwise use node.label as the custom displayName
    let displayName : Option String :=
      if node.label.contains '.' then
        -- Full qualified name like "SBSTest.Chapter2.square_nonneg"
        -- Derive short name from first leanDecl (last component of the name)
        if h : 0 < node.leanDecls.size then
          let declName : Lean.Name := node.leanDecls[0]
          let shortName := declName.components.getLast?.map (·.toString)
          shortName
        else
          some normalizedId  -- Fall back to LaTeX label
      else
        -- Custom displayName was set
        some node.label

    nodes := nodes.push {
      label := normalizedId  -- Use normalized label for consistent lookup
      title := some node.label
      envType := node.envType
      status := node.status
      -- Left column: LaTeX statement and proof (for MathJax rendering)
      statementHtml := artifact.bind (·.latexStatement) |>.getD ""
      proofHtml := artifact.bind (·.latexProof)
      -- Right column: Lean signature and proof body (separate for toggle sync)
      signatureHtml := signatureHtml
      proofBodyHtml := proofBodyHtml
      hoverData := hoverData
      declNames := node.leanDecls
      uses := (depGraph.inEdges node.id).map (·.from_.replace ":" "-")
      url := node.url.replace ":" "-"  -- Normalize URL anchor to match HTML id
      displayName := displayName
      -- Node metadata from manifest
      keyTheorem := keyTheorem
      message := message
      priorityItem := priorityItem
      blocked := blocked
      potentialIssue := potentialIssue
      technicalDebt := technicalDebt
      misc := misc
    }

  IO.println s!"[DEBUG] Finished converting graph nodes: {nodes.size} NodeInfo created"

  -- If no nodes from graph, build from artifacts directly
  let mut finalNodes := nodes
  if nodes.isEmpty && !declArtifacts.isEmpty then
    IO.println s!"[DEBUG] No graph nodes, building from {declArtifacts.size} artifacts directly..."
    for (key, art) in declArtifacts.toArray do
      -- Extract Lean signature and proof body HTML separately for right column
      let signatureHtml := art.leanSignatureHtml.filter (·.isEmpty == false)
      let proofBodyHtml := art.leanProofBodyHtml.filter (·.isEmpty == false)

      -- Get node metadata from manifest
      let keyTheorem := manifest.isKeyTheorem key
      let message := manifest.getMessage key
      let priorityItem := manifest.getPriorityItem key
      let blocked := manifest.getBlocked key
      let potentialIssue := manifest.getPotentialIssue key
      let technicalDebt := manifest.getTechnicalDebt key
      let misc := manifest.getMisc key

      -- Derive displayName: short name from art.name (last component) or fall back to key
      let displayName : Option String :=
        if art.name.isEmpty then some key
        else
          let declName := art.name.toName
          declName.components.getLast?.map (·.toString)

      finalNodes := finalNodes.push {
        label := key
        title := if art.name.isEmpty then none else some art.name
        envType := "theorem"  -- Default, will be overridden when graph is available
        status := if art.leanOk then .proven else .stated
        -- Left column: LaTeX statement and proof
        statementHtml := art.latexStatement.getD ""
        proofHtml := art.latexProof
        -- Right column: Lean signature and proof body (separate for toggle sync)
        signatureHtml := signatureHtml
        proofBodyHtml := proofBodyHtml
        hoverData := art.hoverData  -- Already loaded during initial traversal
        declNames := if art.name.isEmpty then #[] else #[art.name.toName]
        uses := art.uses.map (·.replace ":" "-")  -- Normalize dependency labels
        url := s!"#node-{key}"
        displayName := displayName
        -- Node metadata from manifest
        keyTheorem := keyTheorem
        message := message
        priorityItem := priorityItem
        blocked := blocked
        potentialIssue := potentialIssue
        technicalDebt := technicalDebt
        misc := misc
      }

  IO.println s!"[DEBUG] Final nodes count: {finalNodes.size}"

  -- Load chapters from blueprint.tex if configured
  IO.println "[DEBUG] Loading blueprint chapters (LaTeX parsing)..."
  let chapters ← loadBlueprintChapters config finalNodes
  IO.println s!"[DEBUG] Blueprint chapters loaded: {chapters.size} chapters"

  -- Assign display numbers to nodes based on chapter/section structure
  IO.println "[DEBUG] Assigning display numbers..."
  let numberedNodes := assignDisplayNumbers finalNodes chapters
  IO.println "[DEBUG] Display numbers assigned"

  IO.println "[DEBUG] buildSiteFromArtifacts: Complete, returning BlueprintSite"
  return {
    config := config
    nodes := numberedNodes
    depGraph := depGraph
    pages := #[]
    depGraphSvg := depGraphSvg
    depGraphJson := depGraphJson
    chapters := chapters
    precomputedStats := precomputedStats
  }

/-- Execute the build command -/
def runBuild (cliConfig : CLIConfig) : IO UInt32 := do
  IO.println "Runway: Building HTML from Dress artifacts..."
  (← IO.getStdout).flush
  IO.println s!"[DEBUG] runBuild: Starting..."
  (← IO.getStdout).flush
  IO.println "[DEBUG] About to print configPath..."
  (← IO.getStdout).flush
  IO.println s!"[DEBUG] configPath = {cliConfig.configPath}"
  (← IO.getStdout).flush
  IO.println s!"[DEBUG] buildDir = {cliConfig.buildDir}"
  (← IO.getStdout).flush

  -- Load configuration
  IO.println "[DEBUG] Loading config..."
  (← IO.getStdout).flush
  let config ← loadConfig cliConfig.configPath
  IO.println "[DEBUG] Config loaded, about to access fields..."
  (← IO.getStdout).flush
  IO.println s!"[DEBUG] Config loaded. assetsDir = {config.assetsDir}"
  (← IO.getStdout).flush
  IO.println s!"[DEBUG] blueprintTexPath = {config.blueprintTexPath.getD "none"}"
  (← IO.getStdout).flush

  -- Determine directories
  IO.println "[DEBUG] Creating dressedDir path..."
  (← IO.getStdout).flush
  let dressedDir := cliConfig.buildDir / "dressed"
  IO.println "[DEBUG] Creating outputDir path..."
  (← IO.getStdout).flush
  let outputDir := cliConfig.outputDir.getD (cliConfig.buildDir / "runway")
  IO.println s!"[DEBUG] dressedDir = {dressedDir}"
  (← IO.getStdout).flush
  IO.println s!"[DEBUG] outputDir = {outputDir}"
  (← IO.getStdout).flush

  -- Check if dressed directory exists
  IO.println "[DEBUG] Checking if dressed directory exists..."
  (← IO.getStdout).flush
  if !(← dressedDir.pathExists) then
    IO.eprintln s!"Error: Dressed artifacts not found at {dressedDir}"
    IO.eprintln "Run 'lake build' to generate Dress artifacts first."
    return 1
  IO.println "[DEBUG] Dressed directory exists"

  -- Build the site
  IO.println "[DEBUG] Calling buildSiteFromArtifacts..."
  let site ← buildSiteFromArtifacts config dressedDir
  IO.println s!"[DEBUG] buildSiteFromArtifacts returned. nodes={site.nodes.size}, chapters={site.chapters.size}"

  -- Generate HTML output
  IO.println "[DEBUG] Creating output directory..."
  IO.FS.createDirAll outputDir
  IO.println "[DEBUG] Output directory created"

  -- Generate site - multi-page if chapters available, single-page otherwise
  IO.println "[DEBUG] Generating site HTML..."
  if site.chapters.isEmpty then
    IO.println "[DEBUG] Using single-page mode (no chapters)"
    -- Single-page mode (original behavior)
    generateSite defaultTheme site outputDir
  else
    IO.println s!"[DEBUG] Using multi-page mode ({site.chapters.size} chapters)"
    -- Multi-page mode with chapter pages
    generateMultiPageSite defaultTheme site outputDir
  IO.println "[DEBUG] Site HTML generation complete"

  -- Copy assets from config.assetsDir to output
  let assetsOutputDir := outputDir / "assets"
  IO.FS.createDirAll assetsOutputDir

  -- Copy common.css (required - must load before blueprint.css)
  let srcCommonCss := config.assetsDir / "common.css"
  let dstCommonCss := assetsOutputDir / "common.css"
  if ← srcCommonCss.pathExists then
    let commonCssContent ← IO.FS.readFile srcCommonCss
    IO.FS.writeFile dstCommonCss commonCssContent
    IO.println s!"  - Copied {srcCommonCss} to {dstCommonCss}"
  else
    throw <| IO.userError s!"ERROR: Required asset file not found: {srcCommonCss}"

  -- Copy CSS (required - no fallback)
  let srcCss := config.assetsDir / "blueprint.css"
  let dstCss := assetsOutputDir / "blueprint.css"
  if ← srcCss.pathExists then
    let cssContent ← IO.FS.readFile srcCss
    IO.FS.writeFile dstCss cssContent
    IO.println s!"  - Copied {srcCss} to {dstCss}"
  else
    throw <| IO.userError s!"ERROR: Required asset file not found: {srcCss}"

  -- Copy JS files (required - no fallback)
  let srcPlastex := config.assetsDir / "plastex.js"
  let dstPlastex := assetsOutputDir / "plastex.js"
  if ← srcPlastex.pathExists then
    let plastexContent ← IO.FS.readFile srcPlastex
    IO.FS.writeFile dstPlastex plastexContent
    IO.println s!"  - Copied {srcPlastex} to {dstPlastex}"
  else
    throw <| IO.userError s!"ERROR: Required asset file not found: {srcPlastex}"

  let srcVerso := config.assetsDir / "verso-code.js"
  let dstVerso := assetsOutputDir / "verso-code.js"
  if ← srcVerso.pathExists then
    let versoContent ← IO.FS.readFile srcVerso
    IO.FS.writeFile dstVerso versoContent
    IO.println s!"  - Copied {srcVerso} to {dstVerso}"
  else
    throw <| IO.userError s!"ERROR: Required asset file not found: {srcVerso}"

  -- Copy paper.css (optional - for paper page)
  let srcPaperCss := config.assetsDir / "paper.css"
  let dstPaperCss := assetsOutputDir / "paper.css"
  if ← srcPaperCss.pathExists then
    let paperCssContent ← IO.FS.readFile srcPaperCss
    IO.FS.writeFile dstPaperCss paperCssContent
    IO.println s!"  - Copied {srcPaperCss} to {dstPaperCss}"
  -- paper.css is optional, so no error if not found

  IO.println s!"Site generated at {outputDir}"
  IO.println s!"  - {site.nodes.size} nodes"
  IO.println s!"  - {site.depGraph.edges.size} dependency edges"
  if !site.chapters.isEmpty then
    IO.println s!"  - {site.chapters.size} chapters"

  return 0

/-- Execute the serve command -/
def runServe (cliConfig : CLIConfig) : IO UInt32 := do
  let outputDir := cliConfig.outputDir.getD (cliConfig.buildDir / "runway")

  if !(← outputDir.pathExists) then
    IO.eprintln s!"Error: Output directory not found at {outputDir}"
    IO.eprintln "Run 'runway build' first."
    return 1

  IO.println s!"Runway: Starting local server..."
  IO.println s!"Serving files from {outputDir}"
  IO.println "Starting Python HTTP server on http://localhost:8000"
  IO.println "Press Ctrl+C to stop."

  -- Use Python's built-in HTTP server
  let child ← IO.Process.spawn {
    cmd := "python3"
    args := #["-m", "http.server", "8000", "--directory", outputDir.toString]
    stdout := .inherit
    stderr := .inherit
    stdin := .inherit
  }
  let exitCode ← child.wait
  return exitCode

/-- Execute the check command -/
def runCheck (cliConfig : CLIConfig) : IO UInt32 := do
  IO.println "Runway: Checking declarations..."

  let dressedDir := cliConfig.buildDir / "dressed"

  if !(← dressedDir.pathExists) then
    IO.eprintln s!"Error: Dressed artifacts not found at {dressedDir}"
    return 1

  -- Load the dependency graph
  let depGraph ← loadDepGraph dressedDir

  let mut missingDecls := 0
  let mut totalDecls := 0

  for node in depGraph.nodes do
    for decl in node.leanDecls do
      totalDecls := totalDecls + 1
      -- In a full implementation, we would verify that the declaration
      -- actually exists in the Lean environment. For now, just report.
      if node.status == .notReady then
        missingDecls := missingDecls + 1
        IO.eprintln s!"Warning: {decl} is not ready"

  IO.println s!"Checked {totalDecls} declarations"
  if missingDecls > 0 then
    IO.println s!"  - {missingDecls} declarations not ready"
    return 1
  else
    IO.println "  - All declarations verified"
    return 0

/-- Execute the paper command - generate ar5iv-style paper from paper.tex -/
def runPaper (cliConfig : CLIConfig) : IO UInt32 := do
  IO.println "Runway: Generating ar5iv-style paper..."

  -- Load configuration
  let config ← loadConfig cliConfig.configPath

  -- Determine paper.tex path (CLI override or config)
  let paperTexPath := cliConfig.paperTexPath.map toString |>.orElse (fun _ => config.paperTexPath)
  match paperTexPath with
  | none =>
    IO.eprintln "Error: No paper.tex path specified. Set 'paperTexPath' in runway.json or use --paper-tex option."
    return 1
  | some texPathStr =>
    let texPath : FilePath := texPathStr

    -- Check if paper.tex exists
    if !(← texPath.pathExists) then
      IO.eprintln s!"Error: Paper tex file not found at {texPath}"
      return 1

    IO.println s!"  - Parsing {texPath}"

    -- Parse paper.tex
    let (doc, errors) ← parseFile texPath
    for err in errors do
      IO.eprintln s!"    LaTeX parse warning: {err}"

    -- Determine directories
    let dressedDir := cliConfig.buildDir / "dressed"
    let outputDir := cliConfig.outputDir.getD (cliConfig.buildDir / "runway")

    -- Check if dressed directory exists
    if !(← dressedDir.pathExists) then
      IO.eprintln s!"Error: Dressed artifacts not found at {dressedDir}"
      IO.eprintln "Run 'lake build' to generate Dress artifacts first."
      return 1

    -- Load artifacts (reuse the site building logic)
    let site ← buildSiteFromArtifacts config dressedDir
    IO.println s!"  - Loaded {site.nodes.size} nodes from artifacts"

    -- Build artifact map from nodes
    let mut artifacts : HashMap String NodeInfo := {}
    for node in site.nodes do
      artifacts := artifacts.insert node.label node

    -- Convert document to paper HTML
    let docContent := Runway.Paper.convertDocument doc artifacts config

    -- Wrap in ar5iv-paper container
    let paperContent := Runway.Paper.renderPaperContent config docContent

    -- Apply sidebar template (same as other blueprint pages)
    let ctx : Runway.Render.Context := {
      config := config
      depGraph := site.depGraph
      path := #[]
    }
    let paperTemplate := Runway.DefaultTheme.primaryTemplateWithSidebar site.chapters (some "paper")
    let (paperHtml, _) ← paperTemplate paperContent |>.run ctx

    -- Ensure output directory exists
    IO.FS.createDirAll outputDir

    -- Write paper.html
    let paperOutputPath := outputDir / "paper.html"
    IO.FS.writeFile paperOutputPath (Verso.Output.Html.doctype ++ "\n" ++ paperHtml.asString)
    IO.println s!"  - Generated {paperOutputPath}"

    -- Copy/create paper.css in assets
    let assetsOutputDir := outputDir / "assets"
    IO.FS.createDirAll assetsOutputDir

    -- Check for paper.css in assetsDir first
    let srcPaperCss := config.assetsDir / "paper.css"
    let dstPaperCss := assetsOutputDir / "paper.css"
    if ← srcPaperCss.pathExists then
      let cssContent ← IO.FS.readFile srcPaperCss
      IO.FS.writeFile dstPaperCss cssContent
      IO.println s!"  - Copied {srcPaperCss} to {dstPaperCss}"
    else
      -- Generate minimal paper.css if not provided
      let minimalCss := paperCssContent
      IO.FS.writeFile dstPaperCss minimalCss
      IO.println s!"  - Generated minimal {dstPaperCss}"

    IO.println s!"Paper generated at {paperOutputPath}"
    return 0
where
  /-- Minimal paper CSS content -/
  paperCssContent : String :=
    "/* ar5iv-style Paper CSS */
body.ar5iv-paper {
  font-family: 'Computer Modern Serif', 'Latin Modern Roman', Georgia, serif;
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
  line-height: 1.6;
  color: #333;
}
.paper-header { text-align: center; margin-bottom: 3rem; }
.paper-title { font-size: 2rem; margin-bottom: 0.5rem; }
.paper-authors { font-style: italic; margin-bottom: 1rem; }
.paper-abstract { text-align: left; margin: 2rem auto; max-width: 600px; font-size: 0.95rem; }
.paper-content { }
.paper-chapter h1 { font-size: 1.5rem; border-bottom: 1px solid #ccc; padding-bottom: 0.5rem; margin-top: 2rem; }
.paper-section h2 { font-size: 1.25rem; margin-top: 1.5rem; }
.paper-section h3 { font-size: 1.1rem; margin-top: 1.25rem; }
.paper-theorem { margin: 1.5rem 0; padding: 1rem; background: #f8f9fa; border-left: 3px solid #007bff; }
.paper-definition { border-left-color: #28a745; }
.paper-lemma { border-left-color: #6c757d; }
.paper-theorem-header { font-weight: bold; margin-bottom: 0.5rem; }
.paper-theorem-type { }
.paper-theorem-statement { }
.paper-proof { margin: 1rem 0 1.5rem 1rem; font-style: italic; }
.paper-proof-header { font-style: normal; font-weight: bold; }
.paper-qed { float: right; }
.verification-badge { font-size: 0.75rem; margin-left: 0.5rem; padding: 0.1rem 0.4rem; border-radius: 3px; }
.verification-badge.verified { background: #d4edda; color: #155724; }
.verification-badge.in-progress { background: #fff3cd; color: #856404; }
.verification-badge.not-started { background: #f8d7da; color: #721c24; }
.blueprint-link { font-size: 0.8rem; margin-left: 0.5rem; }
.displaymath { margin: 1rem 0; text-align: center; }
.paper-footer { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid #ccc; font-size: 0.85rem; color: #666; text-align: center; }
.paper-error { color: #dc3545; background: #f8d7da; padding: 0.5rem; margin: 0.5rem 0; border-radius: 3px; }
"

/-- Show help message -/
def showHelp : IO Unit := do
  IO.println "Runway - Presentation layer for Lean mathematical blueprints"
  IO.println ""
  IO.println "Usage: runway [options] [command] [config.json]"
  IO.println ""
  IO.println "Commands:"
  IO.println "  build    Generate HTML from Dress artifacts (default)"
  IO.println "  paper    Generate ar5iv-style paper from paper.tex"
  IO.println "  serve    Start local HTTP server for preview"
  IO.println "  check    Verify Lean declarations exist"
  IO.println ""
  IO.println "Options:"
  IO.println "  --build-dir <path>   Lake build directory (default: .lake/build)"
  IO.println "  --output <path>      Output directory (default: .lake/build/runway)"
  IO.println "  -h, --help           Show this help message"
  IO.println "  -v, --version        Show version information"
  IO.println ""
  IO.println "Examples:"
  IO.println "  runway build                    Build site with default config"
  IO.println "  runway build runway.json        Build site with custom config"
  IO.println "  runway --output _site build     Build to custom output directory"
  IO.println "  runway paper                    Generate paper from paper.tex"
  IO.println "  runway serve                    Start local preview server"

/-- Show version information -/
def showVersion : IO Unit := do
  IO.println "Runway 0.1.0"
  IO.println "Presentation layer for Lean mathematical blueprints"

end Runway.CLI

/-- Main entry point -/
def main (args : List String) : IO UInt32 := do
  IO.println "[DEBUG] main: Entry point reached"
  IO.println s!"[DEBUG] main: args = {args}"
  (← IO.getStdout).flush
  match Runway.CLI.parseArgs args with
  | .error msg =>
    IO.eprintln s!"Error: {msg}"
    IO.eprintln "Run 'runway --help' for usage."
    return 1
  | .ok cliConfig =>
    IO.println s!"[DEBUG] main: parseArgs succeeded, command = {cliConfig.command}"
    (← IO.getStdout).flush
    if cliConfig.showHelp then
      Runway.CLI.showHelp
      return 0
    else if cliConfig.showVersion then
      Runway.CLI.showVersion
      return 0
    else
      match cliConfig.command with
      | "build" => Runway.CLI.runBuild cliConfig
      | "paper" => Runway.CLI.runPaper cliConfig
      | "serve" => Runway.CLI.runServe cliConfig
      | "check" => Runway.CLI.runCheck cliConfig
      | cmd =>
        IO.eprintln s!"Unknown command: {cmd}"
        IO.eprintln "Run 'runway --help' for usage."
        return 1
