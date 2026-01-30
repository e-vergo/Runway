/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# doc-gen4 URL Generation

Utilities for generating URLs to doc-gen4 API documentation.

doc-gen4 URL format:
- Base URL: typically `https://leanprover-community.github.io/{project}/`
- Path: `{Module}/{Submodule}.html#{declName}`
- Example: `https://leanprover-community.github.io/mathlib4_docs/Mathlib/Algebra/Group/Basic.html#mul_one`
-/

namespace Runway.DocGen4

open Lean

/-- Convert a Lean name to a doc-gen4 URL path.

Example: `MyProject.Foo.bar` → `MyProject/Foo.html#bar`

The module path consists of all components except the last (the declaration name).
-/
def nameToPath (name : Name) : String :=
  let components := name.components
  match components with
  | [] => ""
  | [single] =>
    -- Single-component name (no module path)
    s!".html#{single}"
  | _ =>
    let moduleParts := components.dropLast
    let declName := components.getLast!
    let modulePath := moduleParts.map toString |> String.intercalate "/"
    s!"{modulePath}.html#{declName}"

/-- Generate a full URL to doc-gen4 documentation.

Combines the base URL with the path generated from the name.
-/
def docUrl (baseUrl : String) (name : Name) : String :=
  let path := nameToPath name
  let normalizedBase := if baseUrl.endsWith "/" then baseUrl.dropEnd 1 else baseUrl
  s!"{normalizedBase}/{path}"

/-- Render a link to doc-gen4 documentation as HTML.

If a base URL is provided, creates an anchor tag with target="_blank".
Otherwise, creates a plain span with the declaration name.
-/
def renderLink (baseUrl : Option String) (name : Name) : String :=
  let nameStr := name.toString
  match baseUrl with
  | some url =>
    let href := docUrl url name
    s!"<a href=\"{href}\" class=\"decl-link\" target=\"_blank\">{nameStr}</a>"
  | none =>
    s!"<span class=\"decl-name\">{nameStr}</span>"

end Runway.DocGen4
