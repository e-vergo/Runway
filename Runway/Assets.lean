/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Static Assets

Assets (CSS, JavaScript) are now loaded from external files specified
in the config's `assetsDir` field rather than embedded as string literals.

See `Main.lean` for the asset copying logic.
-/

namespace Runway.Assets

-- Assets are copied from config.assetsDir by Main.lean
-- No embedded strings needed

end Runway.Assets
