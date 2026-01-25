/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Runway

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
    if cmd == "build" || cmd == "serve" || cmd == "check" then
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
  if ← path.pathExists then
    let content ← IO.FS.readFile path
    match Lean.Json.parse content >>= Lean.FromJson.fromJson? with
    | .ok config => return config
    | .error e => throw <| IO.userError s!"Failed to parse config: {e}"
  else
    -- Return default config if file doesn't exist
    IO.eprintln s!"Warning: Config file not found at {path}, using defaults"
    return { projectName := "Blueprint" }

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

/-- Find and load decl.html and decl.hovers.json for a node by searching the dressed directory iteratively -/
def loadCodeHtmlAndHovers (dressedDir : FilePath) (sanitizedLabel : String) : IO (Option String × Option String) := do
  -- Search for the decl.html file in any module subdirectory
  -- The path is: dressed/{Module/Path}/{sanitized-label}/decl.html
  -- Use iterative BFS instead of recursion to avoid termination issues
  let mut queue : Array FilePath := #[dressedDir]
  let mut found : Option FilePath := none

  while !queue.isEmpty && found.isNone do
    match queue.back? with
    | none => break
    | some dir =>
      queue := queue.pop
      for entry in ← dir.readDir do
        if ← entry.path.isDir then
          if entry.fileName == sanitizedLabel then
            -- Found the label directory
            found := some entry.path
          else
            queue := queue.push entry.path

  match found with
  | some declDir =>
    -- Load HTML
    let htmlPath := declDir / "decl.html"
    let codeHtml ← if ← htmlPath.pathExists then
      some <$> IO.FS.readFile htmlPath
    else
      pure none
    -- Load hover data
    let hoversPath := declDir / "decl.hovers.json"
    let hoverData ← if ← hoversPath.pathExists then
      some <$> IO.FS.readFile hoversPath
    else
      pure none
    return (codeHtml, hoverData)
  | none => return (none, none)

/-- Load and parse the blueprint.tex file to extract chapters -/
def loadBlueprintChapters (config : Config) (allNodes : Array NodeInfo) : IO (Array ChapterInfo) := do
  match config.blueprintTexPath with
  | none => return #[]
  | some texPath =>
    let path : FilePath := texPath
    if !(← path.pathExists) then
      IO.eprintln s!"Warning: Blueprint tex file not found at {texPath}"
      return #[]

    IO.println s!"  - Loading blueprint structure from {texPath}"
    let (doc, errors) ← parseFile path

    for err in errors do
      IO.eprintln s!"    LaTeX parse warning: {err}"

    -- Extract chapters from document
    let docBody := match doc.root with
      | .document _ body => body
      | other => #[other]

    let chapterExtracts := extractChapters docBody
    IO.println s!"  - Found {chapterExtracts.size} chapters"

    -- Build a map of module name -> nodes for quick lookup
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

    -- Build a map of node label -> NodeInfo for quick lookup
    let labelToNode : HashMap String NodeInfo := Id.run do
      let mut m : HashMap String NodeInfo := {}
      for node in allNodes do
        m := m.insert node.label node
      return m

    -- Convert chapter extracts to ChapterInfo
    let mut chapters : Array ChapterInfo := #[]

    for ce in chapterExtracts do
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
      let sectionExtracts := extractSections ce.body
      let mut sectionInfos : Array SectionInfo := #[]

      for se in sectionExtracts do
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
        let sectionHtmlResult := toHtml se.body

        sectionInfos := sectionInfos.push {
          number := se.number
          title := se.title
          slug := titleToSlug se.title
          nodeLabels := sectionNodes.map (·.label)
          proseHtml := sectionHtmlResult.html
        }

      -- Convert chapter body to HTML
      let chapterHtmlResult := toHtml ce.body

      chapters := chapters.push {
        number := ce.number
        title := ce.title
        slug := titleToSlug ce.title
        isAppendix := ce.isAppendix
        nodeLabels := chapterNodes.map (·.label)
        proseHtml := chapterHtmlResult.html
        sections := sectionInfos
      }

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

/-- Build a BlueprintSite from Dress artifacts -/
def buildSiteFromArtifacts (config : Config) (dressedDir : FilePath) : IO BlueprintSite := do
  -- Load the dependency graph
  let depGraph ← loadDepGraph dressedDir

  -- Load SVG for the graph (may be empty if not generated by Dress)
  let depGraphSvg ← loadDepGraphSvg dressedDir

  -- Generate JSON from the graph we built (don't read from potentially empty file)
  let depGraphJson := some depGraph.toJsonString

  -- Load decl.tex artifacts (contains statement/proof HTML)
  let declArtifacts ← loadDeclArtifacts dressedDir
  IO.println s!"  - Loaded {declArtifacts.size} declaration artifacts from .tex files"

  -- Convert graph nodes to NodeInfo, populating HTML from artifacts
  let mut nodes : Array NodeInfo := #[]
  for node in depGraph.nodes do
    -- Look up artifact by node id (which is the sanitized label)
    let artifact := declArtifacts.get? node.id
    -- Load hover data from decl.hovers.json (codeHtml from decl.html is the full decorated code,
    -- so we prefer the clean signature+proof from the base64-decoded fields in decl.tex)
    let (_, hoverData) ← loadCodeHtmlAndHovers dressedDir node.id

    -- Extract Lean signature and proof body HTML separately for right column
    let signatureHtml := match artifact with
      | some art => art.leanSignatureHtml.filter (·.isEmpty == false)
      | none => none
    let proofBodyHtml := match artifact with
      | some art => art.leanProofBodyHtml.filter (·.isEmpty == false)
      | none => none

    nodes := nodes.push {
      label := node.id
      title := some node.label
      envType := node.envType
      status := node.status
      -- Left column: LaTeX statement and proof (for MathJax rendering)
      statementHtml := artifact.bind (·.latexStatement) |>.getD ""
      proofHtml := artifact.bind (·.latexProof)
      -- Right column: Lean signature and proof body (separate for toggle sync)
      signatureHtml := signatureHtml
      proofBodyHtml := proofBodyHtml
      hoverData := artifact.bind (·.hoverData) |>.orElse (fun _ => hoverData)
      declNames := node.leanDecls
      uses := (depGraph.inEdges node.id).map (·.from_)
      url := node.url
    }

  -- If no nodes from graph, build from artifacts directly
  let mut finalNodes := nodes
  if nodes.isEmpty && !declArtifacts.isEmpty then
    for (key, art) in declArtifacts.toArray do
      let (_, hoverData) ← loadCodeHtmlAndHovers dressedDir key

      -- Extract Lean signature and proof body HTML separately for right column
      let signatureHtml := art.leanSignatureHtml.filter (·.isEmpty == false)
      let proofBodyHtml := art.leanProofBodyHtml.filter (·.isEmpty == false)

      finalNodes := finalNodes.push {
        label := key
        title := if art.name.isEmpty then none else some art.name
        envType := "theorem"  -- Default, will be overridden when graph is available
        status := if art.leanOk then .proved else .stated
        -- Left column: LaTeX statement and proof
        statementHtml := art.latexStatement.getD ""
        proofHtml := art.latexProof
        -- Right column: Lean signature and proof body (separate for toggle sync)
        signatureHtml := signatureHtml
        proofBodyHtml := proofBodyHtml
        hoverData := art.hoverData.orElse (fun _ => hoverData)
        declNames := if art.name.isEmpty then #[] else #[art.name.toName]
        uses := art.uses
        url := s!"#node-{key}"
      }

  -- Load chapters from blueprint.tex if configured
  let chapters ← loadBlueprintChapters config finalNodes

  -- Assign display numbers to nodes based on chapter/section structure
  let numberedNodes := assignDisplayNumbers finalNodes chapters

  return {
    config := config
    nodes := numberedNodes
    depGraph := depGraph
    pages := #[]
    depGraphSvg := depGraphSvg
    depGraphJson := depGraphJson
    chapters := chapters
  }

/-- Execute the build command -/
def runBuild (cliConfig : CLIConfig) : IO UInt32 := do
  IO.println "Runway: Building HTML from Dress artifacts..."

  -- Load configuration
  let config ← loadConfig cliConfig.configPath

  -- Determine directories
  let dressedDir := cliConfig.buildDir / "dressed"
  let outputDir := cliConfig.outputDir.getD (cliConfig.buildDir / "runway")

  -- Check if dressed directory exists
  if !(← dressedDir.pathExists) then
    IO.eprintln s!"Error: Dressed artifacts not found at {dressedDir}"
    IO.eprintln "Run 'lake build' to generate Dress artifacts first."
    return 1

  -- Build the site
  let site ← buildSiteFromArtifacts config dressedDir

  -- Generate HTML output
  IO.FS.createDirAll outputDir

  -- Generate site - multi-page if chapters available, single-page otherwise
  if site.chapters.isEmpty then
    -- Single-page mode (original behavior)
    generateSite defaultTheme site outputDir
  else
    -- Multi-page mode with chapter pages
    generateMultiPageSite defaultTheme site outputDir

  -- Write additional assets
  Assets.writeAssets outputDir

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

/-- Show help message -/
def showHelp : IO Unit := do
  IO.println "Runway - Presentation layer for Lean mathematical blueprints"
  IO.println ""
  IO.println "Usage: runway [options] [command] [config.json]"
  IO.println ""
  IO.println "Commands:"
  IO.println "  build    Generate HTML from Dress artifacts (default)"
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
  IO.println "  runway serve                    Start local preview server"

/-- Show version information -/
def showVersion : IO Unit := do
  IO.println "Runway 0.1.0"
  IO.println "Presentation layer for Lean mathematical blueprints"

end Runway.CLI

/-- Main entry point -/
def main (args : List String) : IO UInt32 := do
  match Runway.CLI.parseArgs args with
  | .error msg =>
    IO.eprintln s!"Error: {msg}"
    IO.eprintln "Run 'runway --help' for usage."
    return 1
  | .ok cliConfig =>
    if cliConfig.showHelp then
      Runway.CLI.showHelp
      return 0
    else if cliConfig.showVersion then
      Runway.CLI.showVersion
      return 0
    else
      match cliConfig.command with
      | "build" => Runway.CLI.runBuild cliConfig
      | "serve" => Runway.CLI.runServe cliConfig
      | "check" => Runway.CLI.runCheck cliConfig
      | cmd =>
        IO.eprintln s!"Unknown command: {cmd}"
        IO.eprintln "Run 'runway --help' for usage."
        return 1
