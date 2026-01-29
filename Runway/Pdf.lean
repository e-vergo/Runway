/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# PDF Compilation

This module provides types and functions for compiling LaTeX documents to PDF
using various LaTeX compilers (pdflatex, tectonic, xelatex, lualatex).
-/

namespace Runway.Pdf

/-- Supported LaTeX compilers -/
inductive Compiler
  | pdflatex
  | tectonic
  | xelatex
  | lualatex
  deriving Inhabited, Repr, BEq

instance : ToString Compiler where
  toString
    | .pdflatex => "pdflatex"
    | .tectonic => "tectonic"
    | .xelatex => "xelatex"
    | .lualatex => "lualatex"

/-- Parse compiler name from string -/
def Compiler.fromString? (s : String) : Option Compiler :=
  match s.toLower with
  | "pdflatex" => some .pdflatex
  | "tectonic" => some .tectonic
  | "xelatex" => some .xelatex
  | "lualatex" => some .lualatex
  | _ => none

/-- Configuration for PDF compilation -/
structure PdfConfig where
  /-- The LaTeX compiler to use -/
  compiler : Compiler := .pdflatex
  /-- Number of compilation passes (for cross-references) -/
  passes : Nat := 2
  /-- Whether to keep auxiliary files (.aux, .log, etc.) -/
  keepAuxFiles : Bool := false
  /-- Additional arguments to pass to the compiler -/
  extraArgs : Array String := #[]
  deriving Inhabited, Repr

/-- Result of PDF compilation -/
inductive CompileResult
  /-- Successful compilation -/
  | success (pdfPath : System.FilePath) (logOutput : String)
  /-- Compiler executable not found -/
  | compilerNotFound (compiler : String)
  /-- Compilation failed with error -/
  | compilationFailed (exitCode : UInt32) (logOutput : String)
  /-- Source .tex file not found -/
  | texFileNotFound (path : System.FilePath)
  deriving Inhabited, Repr

/-- Check if a compiler is available on the system -/
def findCompiler (compiler : Compiler) : IO (Option System.FilePath) := do
  let compilerName := toString compiler
  try
    let result ← IO.Process.output {
      cmd := "which"
      args := #[compilerName]
    }
    if result.exitCode == 0 then
      return some ⟨result.stdout.trimAscii.toString⟩
    else
      return none
  catch _ =>
    return none

/-- Detect available compiler, preferring tectonic over pdflatex -/
def detectCompiler : IO (Option Compiler) := do
  -- Try tectonic first (modern, self-contained)
  if (← findCompiler .tectonic).isSome then return some .tectonic
  -- Fall back to pdflatex (most common)
  if (← findCompiler .pdflatex).isSome then return some .pdflatex
  -- Try xelatex for Unicode support
  if (← findCompiler .xelatex).isSome then return some .xelatex
  -- Try lualatex
  if (← findCompiler .lualatex).isSome then return some .lualatex
  return none

/-- Auxiliary file extensions to clean up -/
private def auxExtensions : Array String :=
  #[".aux", ".log", ".toc", ".out", ".bbl", ".blg", ".nav", ".snm", ".vrb", ".fls", ".fdb_latexmk"]

/-- Run the LaTeX compiler on a .tex file -/
def runCompiler (config : PdfConfig) (texPath : System.FilePath) : IO CompileResult := do
  -- Check if tex file exists
  if !(← texPath.pathExists) then
    return .texFileNotFound texPath

  -- Check if compiler is available
  let compilerPath ← findCompiler config.compiler
  if compilerPath.isNone then
    return .compilerNotFound (toString config.compiler)

  let workDir := texPath.parent.getD "."
  let texFileName := texPath.fileName.getD "document.tex"
  let baseName := (texFileName.dropEnd 4).toString  -- Remove .tex

  -- Build compiler arguments based on compiler type
  -- Since we run from workDir (cwd), use just the filename, not full path
  let (compilerCmd, baseArgs) : String × Array String := match config.compiler with
    | .tectonic =>
      -- Tectonic handles multiple passes automatically
      ("tectonic", #["--keep-logs", texFileName])
    | .pdflatex =>
      ("pdflatex", #["-interaction=nonstopmode", texFileName])
    | .xelatex =>
      ("xelatex", #["-interaction=nonstopmode", texFileName])
    | .lualatex =>
      ("lualatex", #["-interaction=nonstopmode", texFileName])

  let args := baseArgs ++ config.extraArgs

  -- Determine number of passes (tectonic handles this automatically)
  let passes := if config.compiler == .tectonic then 1 else config.passes

  -- Run compiler (multiple passes for pdflatex-family to resolve cross-references)
  let mut lastOutput := ""
  let mut lastExitCode : UInt32 := 0

  for _ in [:passes] do
    let result ← IO.Process.output {
      cmd := compilerCmd
      args := args
      cwd := some workDir
    }
    lastOutput := result.stdout ++ result.stderr
    lastExitCode := result.exitCode

    -- For pdflatex-family, only continue if successful
    if result.exitCode != 0 && config.compiler != .tectonic then
      return .compilationFailed result.exitCode lastOutput

  -- Check final exit code
  if lastExitCode != 0 then
    return .compilationFailed lastExitCode lastOutput

  -- Determine output PDF path
  let pdfPath := workDir / (baseName ++ ".pdf")

  -- Check if PDF was actually generated
  if !(← pdfPath.pathExists) then
    return .compilationFailed 1 (lastOutput ++ "\n[PDF file not found after compilation]")

  -- Clean up auxiliary files if requested
  if !config.keepAuxFiles then
    for ext in auxExtensions do
      let auxPath := workDir / (baseName ++ ext)
      if ← auxPath.pathExists then
        try
          IO.FS.removeFile auxPath
        catch _ => pure ()  -- Ignore cleanup errors

  return .success pdfPath lastOutput

/-- Compile a LaTeX string to PDF by writing to a temp file -/
def compileLatexString (latex : String) (outputDir : System.FilePath)
    (baseName : String := "document") (config : PdfConfig := {}) : IO CompileResult := do
  -- Ensure output directory exists
  IO.FS.createDirAll outputDir

  -- Write LaTeX to file
  let texPath := outputDir / (baseName ++ ".tex")
  IO.FS.writeFile texPath latex

  -- Compile
  runCompiler config texPath

end Runway.Pdf
