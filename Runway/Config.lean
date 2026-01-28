/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean.Data.Json.FromToJson

/-!
# Site Configuration

Configuration types for Blueprint site generation.
-/

namespace Runway

open Lean (Json ToJson FromJson)

/-- Configuration for a Blueprint site -/
structure Config where
  /-- The site title shown in the browser -/
  title : String := "Blueprint"
  /-- The Lean project name -/
  projectName : String
  /-- URL to the GitHub repository -/
  githubUrl : Option String := none
  /-- URL to the doc-gen4 documentation -/
  docgen4Url : Option String := none
  /-- Base URL for the site (e.g., "/" or "/my-project/") -/
  baseUrl : String := "/"
  /-- Output directory for generated HTML -/
  outputDir : System.FilePath := "_site"
  /-- Path to the blueprint.tex file for chapter-based navigation -/
  blueprintTexPath : Option String := none
  /-- Directory containing static assets (blueprint.css, plastex.js, verso-code.js) -/
  assetsDir : System.FilePath
  /-- Path to paper.tex for ar5iv-style paper generation -/
  paperTexPath : Option String := none
  /-- Paper title (defaults to site title if not specified) -/
  paperTitle : Option String := none
  /-- Paper authors -/
  paperAuthors : Array String := #[]
  /-- Paper abstract -/
  paperAbstract : Option String := none
  /-- Preferred PDF compiler (pdflatex, tectonic, xelatex, lualatex) -/
  pdfCompiler : Option String := none
  /-- Pre-parsed MathJax macros JSON string (populated at runtime from blueprint.tex) -/
  mathjaxMacrosJson : String := ""
  deriving Inhabited, Repr

instance : ToJson Config where
  toJson c := .mkObj [
    ("title", .str c.title),
    ("projectName", .str c.projectName),
    ("githubUrl", match c.githubUrl with | some u => .str u | none => .null),
    ("docgen4Url", match c.docgen4Url with | some u => .str u | none => .null),
    ("baseUrl", .str c.baseUrl),
    ("outputDir", .str c.outputDir.toString),
    ("blueprintTexPath", match c.blueprintTexPath with | some p => .str p | none => .null),
    ("assetsDir", .str c.assetsDir.toString),
    ("paperTexPath", match c.paperTexPath with | some p => .str p | none => .null),
    ("paperTitle", match c.paperTitle with | some t => .str t | none => .null),
    ("paperAuthors", ToJson.toJson c.paperAuthors),
    ("paperAbstract", match c.paperAbstract with | some a => .str a | none => .null),
    ("pdfCompiler", match c.pdfCompiler with | some p => .str p | none => .null)
  ]

instance : FromJson Config where
  fromJson? j := do
    let title : String ← j.getObjValAs? String "title" <|> pure "Blueprint"
    let projectName : String ← j.getObjValAs? String "projectName"
    let githubUrl : Option String := (j.getObjValAs? String "githubUrl").toOption
    let docgen4Url : Option String := (j.getObjValAs? String "docgen4Url").toOption
    let baseUrl : String ← j.getObjValAs? String "baseUrl" <|> pure "/"
    let outputDir : String ← j.getObjValAs? String "outputDir" <|> pure "_site"
    let blueprintTexPath : Option String := (j.getObjValAs? String "blueprintTexPath").toOption
    let assetsDir : String ← j.getObjValAs? String "assetsDir"
    let paperTexPath : Option String := (j.getObjValAs? String "paperTexPath").toOption
    let paperTitle : Option String := (j.getObjValAs? String "paperTitle").toOption
    let paperAuthors : Array String ← j.getObjValAs? (Array String) "paperAuthors" <|> pure #[]
    let paperAbstract : Option String := (j.getObjValAs? String "paperAbstract").toOption
    let pdfCompiler : Option String := (j.getObjValAs? String "pdfCompiler").toOption
    return {
      title := title
      projectName := projectName
      githubUrl := githubUrl
      docgen4Url := docgen4Url
      baseUrl := baseUrl
      outputDir := outputDir
      blueprintTexPath := blueprintTexPath
      assetsDir := assetsDir
      paperTexPath := paperTexPath
      paperTitle := paperTitle
      paperAuthors := paperAuthors
      paperAbstract := paperAbstract
      pdfCompiler := pdfCompiler
    }

end Runway
