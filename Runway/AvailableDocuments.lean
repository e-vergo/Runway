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
    Detection happens in Main.lean, this structure passes the info to Theme.lean. -/
structure AvailableDocuments where
  /-- LaTeX blueprint (blueprint.html) exists -/
  blueprint : Bool := true
  /-- Verso blueprint (blueprint_verso.html) exists -/
  blueprintVerso : Bool := false
  /-- LaTeX paper (paper.html) exists -/
  paper : Bool := false
  /-- Verso paper (paper_verso.html) exists -/
  paperVerso : Bool := false
  /-- PDF viewer (pdf.html) exists -/
  pdf : Bool := false
  deriving Inhabited, Repr

/-- Default available documents (only blueprint) -/
def AvailableDocuments.default : AvailableDocuments := {}

end Runway
