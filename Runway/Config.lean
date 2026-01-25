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
  deriving Inhabited, Repr

instance : ToJson Config where
  toJson c := .mkObj [
    ("title", .str c.title),
    ("projectName", .str c.projectName),
    ("githubUrl", match c.githubUrl with | some u => .str u | none => .null),
    ("docgen4Url", match c.docgen4Url with | some u => .str u | none => .null),
    ("baseUrl", .str c.baseUrl),
    ("outputDir", .str c.outputDir.toString),
    ("blueprintTexPath", match c.blueprintTexPath with | some p => .str p | none => .null)
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
    return {
      title := title
      projectName := projectName
      githubUrl := githubUrl
      docgen4Url := docgen4Url
      baseUrl := baseUrl
      outputDir := outputDir
      blueprintTexPath := blueprintTexPath
    }

end Runway
