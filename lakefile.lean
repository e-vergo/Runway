import Lake
open System Lake DSL

package Runway where
  -- Runway: Presentation layer for Lean blueprints
  -- Dress prepares the code (highlighting, hovers), Runway presents it (HTML output)

@[default_target]
lean_lib Runway

lean_exe runway where
  root := `Main
  supportInterpreter := true

-- Dress provides: LeanArchitect, SubVerso, Verso
-- Use local path for development; change to git for releases
require Dress from ".." / "Dress"

/-! ## Path Helpers -/

/-- Get the runway output directory.
    Returns `.lake/build/runway/` -/
def getRunwayDir (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "runway"

/-! ## Runway Facet

Generates HTML from LaTeX blueprint sources. -/

/-- Facet that builds HTML from blueprint LaTeX sources.

    **Depends on:** Dress's `blueprint` facet (ensures artifacts are generated)

    **Reads:** `blueprint/src/*.tex` files

    **Writes:** `.lake/build/runway/` with HTML output -/
library_facet runway (lib : LeanLib) : Unit := do
  let ws ← getWorkspace
  -- First ensure Dress artifacts are built
  let dressJob ← fetch <| lib.facet `blueprint
  dressJob.mapM fun _ => do
    let buildDir := ws.root.buildDir
    let runwayDir := getRunwayDir buildDir
    IO.FS.createDirAll runwayDir
    -- TODO: Parse LaTeX and generate HTML
    IO.println s!"Runway: would generate HTML to {runwayDir}"

/-! ## CLI Scripts -/

open IO.Process in
/-- Run a command and throw on failure. -/
private def runCmd (cmd : String) (args : Array String) : ScriptM Unit := do
  let child ← spawn { cmd, args, stdout := .inherit, stderr := .inherit, stdin := .null }
  let exitCode ← child.wait
  if exitCode != 0 then
    throw <| IO.userError s!"Error running command {cmd} {args.toList}"

/-- Build HTML from blueprint sources.

    Usage: `lake run runway build` -/
script build (_args : List String) do
  IO.println "Runway: Building HTML from blueprint sources..."
  -- TODO: Implement build logic
  return 0

/-- Serve the generated HTML locally.

    Usage: `lake run runway serve` -/
script serve (_args : List String) do
  IO.println "Runway: Starting local server..."
  -- TODO: Implement HTTP server
  return 0

/-- Check that all Lean declarations exist.

    Usage: `lake run runway check` -/
script check (_args : List String) do
  IO.println "Runway: Checking declarations..."
  -- TODO: Implement declaration checking
  return 0
