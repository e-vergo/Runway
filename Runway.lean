/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Runway.Latex
import Runway.Html
import Runway.Dress

/-!
# Runway

Runway is the presentation layer for Lean mathematical blueprints.
It consumes artifacts from Dress and generates interactive HTML output.

## Architecture

```
Dress (highlighting, hovers, graphs) → Runway (HTML generation) → Interactive website
```

## Modules

- `Runway.Latex`: LaTeX parsing (lexer, parser, AST)
- `Runway.Html`: HTML generation
- `Runway.Dress`: Dress artifact loading (includes graph artifacts from Dress.Graph)
-/
