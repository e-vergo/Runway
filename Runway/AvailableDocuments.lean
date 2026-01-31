/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Available Documents

Tracks which document types are available for sidebar rendering.
Detection happens in Main.lean, this structure passes the info to rendering functions.
-/

namespace Runway

/-- Tracks which document types are available for sidebar rendering.
    Detection happens in Main.lean, this structure passes the info to Theme.lean.

    The sidebar displays 6 document types in two groups:
    - TeX Documents: Blueprint, Paper (web), Paper (pdf)
    - Verso Documents: Blueprint, Paper (web), Paper (pdf)

    Unavailable documents are shown with disabled styling. -/
structure AvailableDocuments where
  /-- TeX blueprint (index.html) - always true -/
  blueprintTex : Bool := true
  /-- TeX paper web version (paper_tex.html) -/
  paperWebTex : Bool := false
  /-- TeX paper PDF viewer (pdf_tex.html) -/
  paperPdfTex : Bool := false
  /-- Verso blueprint (blueprint_verso.html) -/
  blueprintVerso : Bool := false
  /-- Verso paper web version (paper_verso.html) -/
  paperWebVerso : Bool := false
  /-- Verso paper PDF viewer (pdf_verso.html) -/
  paperPdfVerso : Bool := false
  deriving Inhabited, Repr

/-- Default available documents (only blueprint) -/
def AvailableDocuments.default : AvailableDocuments := {}

end Runway
