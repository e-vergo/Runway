/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Runway.Config
import Runway.Graph
import Runway.Genre
import Runway.Site
import Runway.Latex
import Runway.Html
import Runway.Dress
import Runway.Traverse
import Runway.Render
import Runway.Theme
import Runway.Templates
import Runway.DepGraph
import Runway.DocGen4
import Runway.Doc
import Runway.Assets
import Runway.Paper
import Runway.Macros
import Runway.VersoPaper

/-!
# Runway

Runway is the presentation layer for Lean mathematical blueprints.
It consumes artifacts from Dress and generates interactive HTML output.

## Architecture

```
Dress (highlighting, hovers, graphs) → Runway (HTML generation) → Interactive website
```

## Modules

- `Runway.Config`: Site configuration types
- `Runway.Graph`: Dependency graph types (NodeStatus, Graph, Node, Edge)
- `Runway.Genre`: Verso Genre definition for Blueprint documents
- `Runway.Site`: Site structure types
- `Runway.Latex`: LaTeX parsing (lexer, parser, AST)
- `Runway.Html`: HTML generation (LaTeX AST to HTML)
- `Runway.Dress`: Dress artifact loading
- `Runway.Traverse`: TraverseM monad for loading and accessing Dress artifacts
- `Runway.Render`: HtmlT-based rendering for Blueprint nodes and pages
- `Runway.Theme`: Theme system for customizing site appearance
- `Runway.Templates`: Reusable HTML template components
- `Runway.DepGraph`: Dependency graph SVG/JSON loading and embedding
- `Runway.DocGen4`: doc-gen4 URL generation for API documentation links
-/
