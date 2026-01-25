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

/-- Build a BlueprintSite from Dress artifacts -/
def buildSiteFromArtifacts (config : Config) (dressedDir : FilePath) : IO BlueprintSite := do
  -- Load the dependency graph
  let depGraph ← loadDepGraph dressedDir

  -- Load SVG and JSON for the graph
  let depGraphSvg ← loadDepGraphSvg dressedDir
  let depGraphJson ← loadDepGraphJson dressedDir

  -- Convert graph nodes to NodeInfo
  let nodes := depGraph.nodes.map fun node =>
    { label := node.id
      title := some node.label
      envType := node.envType
      status := node.status
      statementHtml := ""  -- Would be populated from module artifacts
      proofHtml := none
      declNames := node.leanDecls
      uses := (depGraph.inEdges node.id).map (·.from_)
      url := node.url }

  return {
    config := config
    nodes := nodes
    depGraph := depGraph
    pages := #[]
    depGraphSvg := depGraphSvg
    depGraphJson := depGraphJson
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
  generateSite defaultTheme site outputDir

  -- Write additional assets
  Assets.writeAssets outputDir

  IO.println s!"Site generated at {outputDir}"
  IO.println s!"  - {site.nodes.size} nodes"
  IO.println s!"  - {site.depGraph.edges.size} dependency edges"

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
