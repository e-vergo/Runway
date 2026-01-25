/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Runway

/-!
# Runway CLI

Main entry point for the Runway command-line tool.
-/

def main (args : List String) : IO UInt32 := do
  match args with
  | ["build"] =>
    IO.println "Runway: Building HTML from blueprint sources..."
    -- TODO: Implement build
    return 0
  | ["serve"] =>
    IO.println "Runway: Starting local server..."
    -- TODO: Implement serve
    return 0
  | ["check"] =>
    IO.println "Runway: Checking declarations..."
    -- TODO: Implement check
    return 0
  | ["--version"] | ["-v"] =>
    IO.println "Runway 0.1.0"
    return 0
  | ["--help"] | ["-h"] | [] =>
    IO.println "Runway - Presentation layer for Lean mathematical blueprints"
    IO.println ""
    IO.println "Usage: runway <command>"
    IO.println ""
    IO.println "Commands:"
    IO.println "  build   Generate HTML from blueprint sources"
    IO.println "  serve   Start local HTTP server for preview"
    IO.println "  check   Verify Lean declarations exist"
    IO.println ""
    IO.println "Options:"
    IO.println "  -h, --help     Show this help"
    IO.println "  -v, --version  Show version"
    return 0
  | _ =>
    IO.eprintln s!"Unknown command: {args}"
    IO.eprintln "Run 'runway --help' for usage."
    return 1
