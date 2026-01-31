/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Verso.Output.Html
import Runway.Config
import Runway.Genre
import Runway.Site
import Runway.Render
import Runway.DepGraph
import Runway.Macros
import Runway.AvailableDocuments

/-!
# Blueprint Theme System

Provides theming support for Blueprint HTML rendering, following
VersoBlog's Theme pattern. Themes control the overall appearance
and layout of the generated site.

## Architecture

A Theme consists of:
- Primary template: wraps entire page content (html, head, body)
- Node template: renders individual blueprint nodes
- Index template: renders the site overview/landing page
- CSS and JS files to include

Themes can be customized or replaced entirely.
-/

namespace Runway

open Lean (Name)
open Verso.Output Html

/-! ## Theme Structure -/

/-- A template function that transforms content to HTML -/
def Template := Html → RenderM Html

/-- A template for rendering nodes -/
def NodeTemplate := NodeInfo → RenderM Html

/-- A template for rendering the site index -/
def IndexTemplate := BlueprintSite → RenderM Html

/-- Theme specification for Blueprint sites -/
structure Theme where
  /-- Theme name for identification -/
  name : String
  /-- Primary template wrapping the entire page -/
  primaryTemplate : Template
  /-- Template for rendering individual nodes -/
  nodeTemplate : NodeTemplate
  /-- Template for rendering the index page -/
  indexTemplate : IndexTemplate
  /-- CSS files to include (filename, content) -/
  cssFiles : Array (String × String) := #[]
  /-- JS files to include (filename, content, inHead) -/
  jsFiles : Array (String × String × Bool) := #[]

/-! ## Default Theme -/

namespace DefaultTheme

/-- Render sidebar navigation -/
def renderSidebar (chapters : Array ChapterInfo) (currentSlug : Option String) (toRoot : String) (config : Config) (availDocs : AvailableDocuments := {}) : Html :=
  -- Dashboard (renamed from "Blueprint Home")
  let dashboardClass := if currentSlug.isNone then "sidebar-item active" else "sidebar-item"
  let dashboardItem := .tag "li" #[] (
    .tag "a" #[("href", s!"{toRoot}index.html"), ("class", dashboardClass)] (Html.text true "Dashboard")
  )

  -- Dependency graph link
  let graphClass := if currentSlug == some "dep_graph" then "sidebar-item active" else "sidebar-item"
  let graphItem := .tag "li" #[] (
    .tag "a" #[("href", s!"{toRoot}dep_graph.html"), ("class", graphClass)] (Html.text true "Dependency Graph")
  )

  -- Separator element
  let separator := .tag "li" #[("class", "nav-separator")] Html.empty

  -- Helper to create a sidebar document item (simple, non-expandable)
  -- available=false -> disabled (grayed, unclickable)
  -- available=true + matches currentSlug -> active (highlighted)
  -- available=true + doesn't match -> normal
  let mkDocItem (label : String) (href : String) (slug : String) (available : Bool) : Html :=
    if available then
      let isActive := currentSlug == some slug
      let cls := if isActive then "sidebar-item active" else "sidebar-item"
      .tag "li" #[] (
        .tag "a" #[("href", s!"{toRoot}{href}"), ("class", cls)] (Html.text true label)
      )
    else
      .tag "li" #[] (
        .tag "span" #[("class", "sidebar-item disabled")] (Html.text true label)
      )

  -- Helper to create an expandable sidebar document item with nested chapter list
  let mkExpandableDocItem (label : String) (href : String) (slug : String) (available : Bool) (docId : String) (chapterItems : Array Html) : Html :=
    if available then
      let isActive := currentSlug == some slug
      let cls := if isActive then "sidebar-item active" else "sidebar-item"
      -- Expandable item with arrow, link, and nested chapter list
      .tag "li" #[("class", "sidebar-item expandable")] (
        .tag "div" #[("class", "item-header")] (
          .tag "span" #[("class", "expand-arrow"), ("onclick", "toggleExpand(this)")] (Html.text true "▶") ++
          .tag "a" #[("href", s!"{toRoot}{href}"), ("class", cls)] (Html.text true label)
        ) ++
        .tag "ul" #[("class", "chapter-list collapsed"), ("data-doc", docId)] (
          .seq chapterItems
        )
      )
    else
      -- Disabled items don't need expand functionality
      .tag "li" #[] (
        .tag "span" #[("class", "sidebar-item disabled")] (Html.text true label)
      )

  -- Build chapter list items for Blueprint [TeX]
  let blueprintChapterItems := chapters.map fun chap =>
    let chapterLabel := if chap.isAppendix
      then s!"Appendix: {chap.title}"
      else s!"Ch {chap.number}: {chap.title}"
    .tag "li" #[] (
      .tag "a" #[("href", s!"{toRoot}{chap.slug}.html")] (Html.text true chapterLabel)
    )

  -- TeX document items (Paper_web, Paper_pdf, Blueprint with chapters)
  let texPaperWeb := mkDocItem "Paper_web [TeX]" "paper_tex.html" "paper_tex" availDocs.paperWebTex
  let texPaperPdf := mkDocItem "Paper_pdf [TeX]" "pdf_tex.html" "pdf_tex" availDocs.paperPdfTex
  -- Blueprint [TeX] is expandable with chapter list
  let texBlueprint := if chapters.isEmpty
    then mkDocItem "Blueprint [TeX]" "index.html" "" availDocs.blueprintTex
    else mkExpandableDocItem "Blueprint [TeX]" "index.html" "" availDocs.blueprintTex "blueprint-tex" blueprintChapterItems

  -- Verso document items (Paper_web, Paper_pdf, Blueprint)
  let versoPaperWeb := mkDocItem "Paper_web [Verso]" "paper_verso.html" "paper_verso" availDocs.paperWebVerso
  let versoPaperPdf := mkDocItem "Paper_pdf [Verso]" "pdf_verso.html" "pdf_verso" availDocs.paperPdfVerso
  let versoBlueprint := mkDocItem "Blueprint [Verso]" "blueprint_verso.html" "blueprint_verso" availDocs.blueprintVerso

  -- External links (API Docs, GitHub)
  let docsItem := match config.docgen4Url with
    | some url => .tag "li" #[] (.tag "a" #[("href", url), ("target", "_blank"), ("class", "sidebar-item")] (Html.text true "API Docs"))
    | none => Html.empty

  let githubItem := match config.githubUrl with
    | some url => .tag "li" #[] (.tag "a" #[("href", url), ("target", "_blank"), ("class", "sidebar-item")] (Html.text true "GitHub"))
    | none => Html.empty

  -- Theme toggle element (will be pushed to bottom via flex spacer)
  let themeToggle := divClass "theme-toggle" (
    .tag "span" #[("class", "theme-toggle-icon sun")] (Html.text true "☀") ++
    .tag "span" #[("class", "theme-toggle-switch")] Html.empty ++
    .tag "span" #[("class", "theme-toggle-icon moon")] (Html.text true "☾")
  )

  -- Flex spacer to push toggle to bottom
  let flexSpacer := .tag "li" #[("class", "sidebar-spacer")] Html.empty

  -- Build nav items in new order:
  -- Dashboard, Dependency Graph, separator
  -- Paper_web [TeX], Paper_pdf [TeX], Blueprint [TeX] (with chapters)
  -- Paper_web [Verso], Paper_pdf [Verso], Blueprint [Verso]
  -- separator, API Docs, GitHub
  -- spacer (flex grow)
  let navItems := #[dashboardItem, graphItem, separator] ++
    #[texPaperWeb, texPaperPdf, texBlueprint] ++
    #[versoPaperWeb, versoPaperPdf, versoBlueprint] ++
    #[separator, docsItem, githubItem, flexSpacer]

  .tag "nav" #[("class", "toc")] (
    .tag "ul" #[("class", "sub-toc-0")] (.seq navItems) ++
    themeToggle
  )

/-- Render prev/next navigation links -/
def renderPrevNextNav (chapters : Array ChapterInfo) (currentSlug : String) (toRoot : String) : Html :=
  -- Find current chapter index
  let currentIdx := chapters.findIdx? (·.slug == currentSlug)
  match currentIdx with
  | none => Html.empty
  | some idx =>
    let prevLink := if idx > 0 then
      let prev := chapters[idx - 1]!
      .tag "a" #[("href", s!"{toRoot}{prev.slug}.html"), ("class", "prev")] (
        Html.text true s!"← {prev.title}"
      )
    else
      .tag "a" #[("href", s!"{toRoot}index.html"), ("class", "prev")] (
        Html.text true "← Home"
      )

    let nextLink := if idx + 1 < chapters.size then
      let next := chapters[idx + 1]!
      .tag "a" #[("href", s!"{toRoot}{next.slug}.html"), ("class", "next")] (
        Html.text true s!"{next.title} →"
      )
    else
      Html.empty

    .tag "nav" #[("class", "prev-next-nav")] (prevLink ++ nextLink)

/-- The primary template wrapping entire page content (plasTeX-compatible structure) -/
def primaryTemplate : Template := fun content => do
  let config ← Render.getConfig
  let toRoot ← Render.pathToRoot

  -- MathJax configuration script (with macros from blueprint.tex)
  let mathjaxConfig := .tag "script" #[] (Html.text false (Macros.generateMathJaxConfig config.mathjaxMacrosJson))

  return .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true config.title) ++
      -- Local CSS (common.css must load before blueprint.css)
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/common.css")] Html.empty ++
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/blueprint.css")] Html.empty ++
      .tag "link" #[("rel", "icon"), ("href", "data:,")] Html.empty ++
      -- MathJax config and script
      mathjaxConfig ++
      .tag "script" #[("id", "MathJax-script"), ("async", ""),
                      ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] Html.empty ++
      -- Popper.js (required by Tippy)
      .tag "script" #[("src", "https://unpkg.com/@popperjs/core@2")] Html.empty ++
      -- Tippy.js for hover tooltips
      .tag "script" #[("src", "https://unpkg.com/tippy.js@6")] Html.empty ++
      .tag "link" #[("rel", "stylesheet"),
                    ("href", "https://unpkg.com/tippy.js@6/themes/light-border.css")] Html.empty ++
      -- marked.js for docstring rendering
      .tag "script" #[("src", "https://cdn.jsdelivr.net/npm/marked/marked.min.js")] Html.empty
    ) ++
    .tag "body" #[] (
      -- plasTeX header with blue gradient
      .tag "header" #[] (
        .tag "nav" #[("class", "header")] (
          divClass "nav-wrapper" (
            -- Hamburger menu for mobile (left side)
            .tag "span" #[("id", "toc-toggle")] (Html.text true "☰")
          )
        )
      ) ++
      -- Main wrapper with sidebar and content
      divClass "wrapper" (
        -- Table of contents sidebar (default single-page version)
        .tag "nav" #[("class", "toc")] (
          .tag "ul" #[("class", "sub-toc-0")] (
            .tag "li" #[("class", "active")] (
              .tag "a" #[("href", s!"{toRoot}index.html")] (Html.text true "Blueprint Home")
            )
          ) ++
          -- Theme toggle
          divClass "theme-toggle" (
            .tag "span" #[("class", "theme-toggle-icon sun")] (Html.text true "☀") ++
            .tag "span" #[("class", "theme-toggle-switch")] Html.empty ++
            .tag "span" #[("class", "theme-toggle-icon moon")] (Html.text true "☾")
          )
        ) ++
        -- Main content area
        divClass "content" content
      ) ++
      -- jQuery (for proof toggles)
      .tag "script" #[("src", "https://code.jquery.com/jquery-3.7.1.min.js"),
                      ("integrity", "sha256-/JqT3SQfawRcv/BIHPThkBvs0OEvtFFmqPF/lYI/Cxo="),
                      ("crossorigin", "anonymous")] Html.empty ++
      -- Local JavaScript
      .tag "script" #[("src", s!"{toRoot}assets/plastex.js")] Html.empty ++
      .tag "script" #[("src", s!"{toRoot}assets/verso-code.js")] Html.empty
    )
  )

/-- Primary template with chapter sidebar navigation -/
def primaryTemplateWithSidebar (chapters : Array ChapterInfo) (currentSlug : Option String) (availDocs : AvailableDocuments := {}) : Template := fun content => do
  let config ← Render.getConfig
  let toRoot ← Render.pathToRoot

  -- MathJax configuration script (with macros from blueprint.tex)
  let mathjaxConfig := .tag "script" #[] (Html.text false (Macros.generateMathJaxConfig config.mathjaxMacrosJson))

  -- Build sidebar
  let sidebar := renderSidebar chapters currentSlug toRoot config availDocs

  -- Build prev/next nav for chapter pages
  let prevNextNav := match currentSlug with
    | some slug => renderPrevNextNav chapters slug toRoot
    | none => Html.empty

  -- Check if this is the paper page to load additional CSS
  let isPaperPage := currentSlug == some "paper"
  let paperCssLink := if isPaperPage then
    .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/paper.css")] Html.empty
  else
    Html.empty

  return .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true config.title) ++
      -- Local CSS (common.css must load before blueprint.css and paper.css)
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/common.css")] Html.empty ++
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/blueprint.css")] Html.empty ++
      paperCssLink ++
      .tag "link" #[("rel", "icon"), ("href", "data:,")] Html.empty ++
      -- MathJax config and script
      mathjaxConfig ++
      .tag "script" #[("id", "MathJax-script"), ("async", ""),
                      ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] Html.empty ++
      -- Popper.js (required by Tippy)
      .tag "script" #[("src", "https://unpkg.com/@popperjs/core@2")] Html.empty ++
      -- Tippy.js for hover tooltips
      .tag "script" #[("src", "https://unpkg.com/tippy.js@6")] Html.empty ++
      .tag "link" #[("rel", "stylesheet"),
                    ("href", "https://unpkg.com/tippy.js@6/themes/light-border.css")] Html.empty ++
      -- marked.js for docstring rendering
      .tag "script" #[("src", "https://cdn.jsdelivr.net/npm/marked/marked.min.js")] Html.empty
    ) ++
    .tag "body" #[] (
      -- plasTeX header with blue gradient
      .tag "header" #[] (
        .tag "nav" #[("class", "header")] (
          divClass "nav-wrapper" (
            -- Hamburger menu for mobile (left side)
            .tag "span" #[("id", "toc-toggle")] (Html.text true "☰")
          )
        )
      ) ++
      -- Main wrapper with sidebar and content
      divClass "wrapper" (
        -- Chapter sidebar navigation
        sidebar ++
        -- Main content area with prev/next nav
        divClass "content" (content ++ prevNextNav)
      ) ++
      -- jQuery (for proof toggles)
      .tag "script" #[("src", "https://code.jquery.com/jquery-3.7.1.min.js"),
                      ("integrity", "sha256-/JqT3SQfawRcv/BIHPThkBvs0OEvtFFmqPF/lYI/Cxo="),
                      ("crossorigin", "anonymous")] Html.empty ++
      -- Local JavaScript
      .tag "script" #[("src", s!"{toRoot}assets/plastex.js")] Html.empty ++
      .tag "script" #[("src", s!"{toRoot}assets/verso-code.js")] Html.empty
    )
  )

/-- Template for rendering individual nodes -/
def nodeTemplate : NodeTemplate := renderNode

/-- Template for rendering the index page -/
def indexTemplate : IndexTemplate := renderIndex

/-- Render full-page PDF viewer with embedded PDF -/
def renderPdfPage (chapters : Array ChapterInfo) (config : Config) (availDocs : AvailableDocuments := {}) : Html :=
  let toRoot := ""

  -- MathJax configuration script (with macros from blueprint.tex for consistency)
  let mathjaxConfig := .tag "script" #[] (Html.text false (Macros.generateMathJaxConfig config.mathjaxMacrosJson))

  -- Build sidebar
  let sidebar := renderSidebar chapters (some "pdf") toRoot config availDocs

  -- PDF viewer content
  let pdfContent := divClass "pdf-viewer-container" (
    .tag "h1" #[] (Html.text true "PDF Document") ++
    .tag "p" #[("class", "pdf-description")] (
      Html.text true "View or download the compiled PDF version of this document."
    ) ++
    .tag "div" #[("class", "pdf-actions")] (
      .tag "a" #[("href", "paper.pdf"), ("download", ""), ("class", "pdf-download-btn")] (
        Html.text true "Download PDF"
      )
    ) ++
    .tag "embed" #[
      ("src", "paper.pdf"),
      ("type", "application/pdf"),
      ("width", "100%"),
      ("height", "800px"),
      ("class", "pdf-embed")
    ] Html.empty ++
    .tag "p" #[("class", "pdf-fallback")] (
      Html.text true "If the PDF does not display, you can " ++
      .tag "a" #[("href", "paper.pdf")] (Html.text true "download it directly") ++
      Html.text true "."
    )
  )

  .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true (config.title ++ " - PDF")) ++
      -- Local CSS
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/common.css")] Html.empty ++
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/blueprint.css")] Html.empty ++
      .tag "link" #[("rel", "icon"), ("href", "data:,")] Html.empty ++
      -- Inline styles for PDF page
      .tag "style" #[] (Html.text false "
        .pdf-viewer-container {
          padding: 1rem;
        }
        .pdf-viewer-container h1 {
          margin-bottom: 0.5rem;
        }
        .pdf-description {
          color: #666;
          margin-bottom: 1rem;
        }
        .pdf-actions {
          margin-bottom: 1rem;
        }
        .pdf-download-btn {
          display: inline-block;
          padding: 0.5rem 1rem;
          background: #007bff;
          color: white;
          text-decoration: none;
          border-radius: 4px;
        }
        .pdf-download-btn:hover {
          background: #0056b3;
        }
        .pdf-embed {
          border: 1px solid #ddd;
          border-radius: 4px;
        }
        .pdf-fallback {
          margin-top: 1rem;
          color: #666;
          font-size: 0.9rem;
        }
      ") ++
      mathjaxConfig ++
      .tag "script" #[("id", "MathJax-script"), ("async", ""),
                      ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] Html.empty
    ) ++
    .tag "body" #[] (
      -- Header
      .tag "header" #[] (
        .tag "nav" #[("class", "header")] (
          divClass "nav-wrapper" (
            .tag "span" #[("id", "toc-toggle")] (Html.text true "☰")
          )
        )
      ) ++
      -- Main wrapper with sidebar and content
      divClass "wrapper" (
        sidebar ++
        divClass "content" pdfContent
      ) ++
      -- jQuery (for sidebar toggle)
      .tag "script" #[("src", "https://code.jquery.com/jquery-3.7.1.min.js"),
                      ("integrity", "sha256-/JqT3SQfawRcv/BIHPThkBvs0OEvtFFmqPF/lYI/Cxo="),
                      ("crossorigin", "anonymous")] Html.empty ++
      -- Local JavaScript
      .tag "script" #[("src", s!"{toRoot}assets/plastex.js")] Html.empty ++
      .tag "script" #[("src", s!"{toRoot}assets/verso-code.js")] Html.empty
    )
  )

/-- Render full-page PDF viewer for Verso-generated PDFs -/
def renderVersoPdfPage (chapters : Array ChapterInfo) (config : Config) (availDocs : AvailableDocuments := {}) : Html :=
  let toRoot := ""

  -- MathJax configuration script (with macros from blueprint.tex for consistency)
  let mathjaxConfig := .tag "script" #[] (Html.text false (Macros.generateMathJaxConfig config.mathjaxMacrosJson))

  -- Build sidebar
  let sidebar := renderSidebar chapters (some "pdf_verso") toRoot config availDocs

  -- PDF viewer content
  let pdfContent := divClass "pdf-viewer-container" (
    .tag "h1" #[] (Html.text true "Verso PDF Document") ++
    .tag "p" #[("class", "pdf-description")] (
      Html.text true "View or download the PDF compiled from Verso source."
    ) ++
    .tag "div" #[("class", "pdf-actions")] (
      .tag "a" #[("href", "paper_verso.pdf"), ("download", ""), ("class", "pdf-download-btn")] (
        Html.text true "Download PDF"
      )
    ) ++
    .tag "embed" #[
      ("src", "paper_verso.pdf"),
      ("type", "application/pdf"),
      ("width", "100%"),
      ("height", "800px"),
      ("class", "pdf-embed")
    ] Html.empty ++
    .tag "p" #[("class", "pdf-fallback")] (
      Html.text true "If the PDF does not display, you can " ++
      .tag "a" #[("href", "paper_verso.pdf")] (Html.text true "download it directly") ++
      Html.text true "."
    )
  )

  .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true (config.title ++ " - Verso PDF")) ++
      -- Local CSS
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/common.css")] Html.empty ++
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}assets/blueprint.css")] Html.empty ++
      .tag "link" #[("rel", "icon"), ("href", "data:,")] Html.empty ++
      -- Inline styles for PDF page
      .tag "style" #[] (Html.text false "
        .pdf-viewer-container {
          padding: 1rem;
        }
        .pdf-viewer-container h1 {
          margin-bottom: 0.5rem;
        }
        .pdf-description {
          color: #666;
          margin-bottom: 1rem;
        }
        .pdf-actions {
          margin-bottom: 1rem;
        }
        .pdf-download-btn {
          display: inline-block;
          padding: 0.5rem 1rem;
          background: #007bff;
          color: white;
          text-decoration: none;
          border-radius: 4px;
        }
        .pdf-download-btn:hover {
          background: #0056b3;
        }
        .pdf-embed {
          border: 1px solid #ddd;
          border-radius: 4px;
        }
        .pdf-fallback {
          margin-top: 1rem;
          color: #666;
          font-size: 0.9rem;
        }
      ") ++
      mathjaxConfig ++
      .tag "script" #[("id", "MathJax-script"), ("async", ""),
                      ("src", "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")] Html.empty
    ) ++
    .tag "body" #[] (
      -- Header
      .tag "header" #[] (
        .tag "nav" #[("class", "header")] (
          divClass "nav-wrapper" (
            .tag "span" #[("id", "toc-toggle")] (Html.text true "☰")
          )
        )
      ) ++
      -- Main wrapper with sidebar and content
      divClass "wrapper" (
        sidebar ++
        divClass "content" pdfContent
      ) ++
      -- jQuery (for sidebar toggle)
      .tag "script" #[("src", "https://code.jquery.com/jquery-3.7.1.min.js"),
                      ("integrity", "sha256-/JqT3SQfawRcv/BIHPThkBvs0OEvtFFmqPF/lYI/Cxo="),
                      ("crossorigin", "anonymous")] Html.empty ++
      -- Local JavaScript
      .tag "script" #[("src", s!"{toRoot}assets/plastex.js")] Html.empty ++
      .tag "script" #[("src", s!"{toRoot}assets/verso-code.js")] Html.empty
    )
  )

end DefaultTheme

/-- The default Blueprint theme (plasTeX-compatible) -/
def defaultTheme : Theme where
  name := "default"
  primaryTemplate := DefaultTheme.primaryTemplate
  nodeTemplate := DefaultTheme.nodeTemplate
  indexTemplate := DefaultTheme.indexTemplate
  -- Assets are copied from config.assetsDir by Main.lean, not embedded here

/-! ## Theme Application -/

/-- Apply a theme to render the complete site -/
def Theme.renderSite (theme : Theme) (site : BlueprintSite) : RenderM Html := do
  -- Render index content (includes embedded graph SVG and JSON data)
  let indexContent ← theme.indexTemplate site

  -- Render all node content
  let nodesContent ← site.nodes.mapM theme.nodeTemplate

  -- Combine and wrap with primary template
  let mainContent := indexContent ++ divClass "all-nodes" (.seq nodesContent)

  -- Apply primary template
  let pageHtml ← theme.primaryTemplate mainContent

  return pageHtml


/-- Generate complete site with theme -/
def generateSite (theme : Theme) (site : BlueprintSite) (outputDir : System.FilePath)
    (availDocs : AvailableDocuments := {}) : IO Unit := do
  -- Create output directory
  IO.FS.createDirAll outputDir

  -- Create render context
  let ctx : Render.Context := {
    config := site.config
    depGraph := site.depGraph
    path := #[]
  }

  -- Render site HTML
  let (html, state) ← theme.renderSite site |>.run ctx

  -- Report any errors
  for err in state.errors do
    IO.eprintln s!"Render warning: {err}"

  -- Write index.html
  let htmlStr := Html.doctype ++ "\n" ++ html.asString
  IO.FS.writeFile (outputDir / "index.html") htmlStr

  -- Generate dedicated dependency graph page with rich modals and sidebar
  let (modalsHtml, _) ← renderAllModals site.nodes |>.run ctx
  let depGraphPage := DepGraph.fullPageGraph site.depGraphSvg site.depGraphJson (some modalsHtml) site.config.title site.chapters (some site.config) availDocs
  let depGraphHtmlStr := Html.doctype ++ "\n" ++ depGraphPage.asString
  IO.FS.writeFile (outputDir / "dep_graph.html") depGraphHtmlStr

  IO.println s!"Site generated at {outputDir}"

/-- Build a lookup map from node label to NodeInfo -/
def buildNodeLookup (nodes : Array NodeInfo) : Std.HashMap String NodeInfo :=
  nodes.foldl (init := {}) fun acc node =>
    acc.insert node.label node

/-- Build a lookup map from module name to nodes in that module.
    Uses the `moduleName` field from dep-graph.json (set by Dress).

    Registers each node under:
    - Full module name: e.g., `PrimeNumberTheoremAnd.Wiener`

    This allows `\inputleanmodule{PrimeNumberTheoremAnd.Wiener}` to match nodes. -/
def buildModuleLookup (_projectName : String) (nodes : Array NodeInfo) : Std.HashMap String (Array NodeInfo) :=
  nodes.foldl (init := {}) fun acc node =>
    if node.moduleName.isEmpty then acc
    else acc.insert node.moduleName (acc.getD node.moduleName #[] |>.push node)

/-- Replace module placeholder divs with rendered nodes from that module.
    Finds `<div class="lean-module-placeholder" data-module="X"></div>` and replaces with
    all rendered nodes from module X. -/
def replaceModulePlaceholders (proseHtml : String)
    (moduleLookup : Std.HashMap String (Array NodeInfo)) : RenderM String := do
  let placeholderPrefix := "<div class=\"lean-module-placeholder\" data-module=\""

  -- Split by the placeholder prefix
  let parts := proseHtml.splitOn placeholderPrefix

  -- First part is always before any placeholder
  if parts.isEmpty then return proseHtml

  let mut result := parts[0]!
  let mut renderedLabels : Std.HashSet String := {}  -- Track rendered nodes to avoid duplicates

  -- Process remaining parts (each starts with: ModuleName"></div>rest...)
  for i in [1:parts.length] do
    let part := parts[i]!
    -- Find the closing quote to extract the module name
    match part.splitOn "\"" with
    | moduleName :: rest =>
      -- rest[0] should be "></div>" and rest[1..] is the content after
      let afterModule := "\"".intercalate rest
      -- Check if it starts with the expected closing
      if afterModule.startsWith "></div>" then
        let afterPlaceholder := afterModule.drop "></div>".length
        -- Look up nodes for this module
        match moduleLookup[moduleName]? with
        | some moduleNodes =>
          -- Render each node from the module (deduplicated)
          let mut nodesHtml := ""
          for node in moduleNodes do
            if !renderedLabels.contains node.label then
              renderedLabels := renderedLabels.insert node.label
              let nodeHtml ← renderNode node
              nodesHtml := nodesHtml ++ nodeHtml.asString
          result := result ++ nodesHtml ++ afterPlaceholder
        | none =>
          -- Module not found, insert warning
          let warningHtml := s!"<div class=\"module-not-found\">Module '{moduleName}' not found or has no nodes</div>"
          result := result ++ warningHtml ++ afterPlaceholder
      else
        -- Malformed placeholder, preserve original
        result := result ++ placeholderPrefix ++ part
    | [] =>
      -- Malformed, preserve original
      result := result ++ placeholderPrefix ++ part

  return result

/-- Replace node placeholder divs with rendered node HTML.
    Finds `<div class="lean-node-placeholder" data-node="X"></div>` and replaces with rendered node.
    Uses splitOn for reliable parsing.
-/
def replaceNodePlaceholders (proseHtml : String) (nodeLookup : Std.HashMap String NodeInfo)
    : RenderM String := do
  let placeholderPrefix := "<div class=\"lean-node-placeholder\" data-node=\""

  -- Split by the placeholder prefix
  let parts := proseHtml.splitOn placeholderPrefix

  -- First part is always before any placeholder
  if parts.isEmpty then return proseHtml

  let mut result := parts[0]!

  -- Process remaining parts (each starts with: LABEL"></div>rest...)
  for i in [1:parts.length] do
    let part := parts[i]!
    -- Find the closing quote to extract the label
    match part.splitOn "\"" with
    | label :: rest =>
      -- rest[0] should be "></div>" and rest[1..] is the content after
      let afterLabel := "\"".intercalate rest
      -- Check if it starts with the expected closing
      if afterLabel.startsWith "></div>" then
        let afterPlaceholder := afterLabel.drop "></div>".length
        -- Look up and render the node (normalize label: colons become hyphens during artifact loading)
        let normalizedLabel := label.replace ":" "-"
        match nodeLookup[normalizedLabel]? with
        | some nodeInfo =>
          let nodeHtml ← renderNode nodeInfo
          let nodeHtmlStr := nodeHtml.asString
          result := result ++ nodeHtmlStr ++ afterPlaceholder
        | none =>
          -- Node not found, insert warning (show original label for clarity)
          let warningHtml := s!"<div class=\"node-not-found\">Node '{label}' (normalized: '{normalizedLabel}') not found</div>"
          result := result ++ warningHtml ++ afterPlaceholder
      else
        -- Malformed placeholder, preserve original
        result := result ++ placeholderPrefix ++ part
    | [] =>
      -- Malformed, preserve original
      result := result ++ placeholderPrefix ++ part

  return result

/-- Replace all placeholder divs (both module and node placeholders) with rendered content.
    First expands module placeholders, then node placeholders. -/
def replacePlaceholders (proseHtml : String) (nodeLookup : Std.HashMap String NodeInfo)
    (moduleLookup : Std.HashMap String (Array NodeInfo)) : RenderM String := do
  -- First replace module placeholders (which expand to multiple nodes)
  let afterModules ← replaceModulePlaceholders proseHtml moduleLookup
  -- Then replace individual node placeholders
  replaceNodePlaceholders afterModules nodeLookup

/-- Render a chapter page content -/
def renderChapterContent (chapter : ChapterInfo) (allNodes : Array NodeInfo) : RenderM Html := do
  -- Get config for project name
  let config ← Render.getConfig
  -- Build lookups for placeholder resolution
  let nodeLookup := buildNodeLookup allNodes
  let moduleLookup := buildModuleLookup config.projectName allNodes

  -- Chapter title
  let titlePrefix := if chapter.isAppendix then "Appendix" else s!"Chapter {chapter.number}"
  let chapterTitle := .tag "h1" #[("class", "chapter-title")] (
    Html.text true s!"{titlePrefix}: {chapter.title}"
  )

  -- Prose content with placeholders resolved to actual rendered nodes
  let resolvedProseHtml ← replacePlaceholders chapter.proseHtml nodeLookup moduleLookup
  let proseHtml := Html.text false resolvedProseHtml

  -- Render sections
  let mut sectionHtmls : Array Html := #[]
  for sec in chapter.sections do
    let sectionTitle := match sec.number with
      | some n => .tag "h2" #[("class", "section-title")] (Html.text true s!"{chapter.number}.{n} {sec.title}")
      | none => .tag "h2" #[("class", "section-title")] (Html.text true sec.title)

    -- Section prose with placeholders resolved
    let resolvedSectionProseHtml ← replacePlaceholders sec.proseHtml nodeLookup moduleLookup
    let sectionProseHtml := Html.text false resolvedSectionProseHtml

    sectionHtmls := sectionHtmls.push (
      divClass "section-wrapper" (
        sectionTitle ++
        sectionProseHtml
      )
    )

  return divClass "chapter-page" (
    chapterTitle ++
    proseHtml ++
    .seq sectionHtmls
  )

/-- Render multi-page index with chapter list -/
def renderMultiPageIndex (site : BlueprintSite) : RenderM Html := do
  let config ← Render.getConfig

  -- Title section (external links are in the sidebar, not here)
  let titleSection := divClass "index-header" (
    .tag "h1" #[] (Html.text true config.title)
  )

  -- Dashboard section (2x2 grid with progress, key theorems, messages, notes)
  let dashboard := renderDashboard site

  return divClass "index-page" (
    titleSection ++
    dashboard
  )

/-- Generate multi-page site with chapter-based navigation -/
def generateMultiPageSite (_theme : Theme) (site : BlueprintSite) (outputDir : System.FilePath) (availDocs : AvailableDocuments := {}) : IO Unit := do
  -- Create output directory
  IO.FS.createDirAll outputDir

  -- Create render context
  let ctx : Render.Context := {
    config := site.config
    depGraph := site.depGraph
    path := #[]
  }

  -- Generate index.html with chapter list
  let (indexContent, _) ← renderMultiPageIndex site |>.run ctx
  let templateWithSidebar := DefaultTheme.primaryTemplateWithSidebar site.chapters none availDocs
  let (indexHtml, _) ← templateWithSidebar indexContent |>.run ctx
  let indexHtmlStr := Html.doctype ++ "\n" ++ indexHtml.asString
  IO.FS.writeFile (outputDir / "index.html") indexHtmlStr
  IO.println s!"  Generated index.html"

  -- Generate chapter pages
  for chap in site.chapters do
    let (chapterContent, _) ← renderChapterContent chap site.nodes |>.run ctx
    let chapterTemplate := DefaultTheme.primaryTemplateWithSidebar site.chapters (some chap.slug) availDocs
    let (chapterHtml, _) ← chapterTemplate chapterContent |>.run ctx
    let chapterHtmlStr := Html.doctype ++ "\n" ++ chapterHtml.asString
    IO.FS.writeFile (outputDir / s!"{chap.slug}.html") chapterHtmlStr
    IO.println s!"  Generated {chap.slug}.html"

  -- Generate dedicated dependency graph page with rich modals and sidebar
  let (modalsHtml, _) ← renderAllModals site.nodes |>.run ctx
  let depGraphPage := DepGraph.fullPageGraph site.depGraphSvg site.depGraphJson (some modalsHtml) site.config.title site.chapters (some site.config) availDocs
  let depGraphHtmlStr := Html.doctype ++ "\n" ++ depGraphPage.asString
  IO.FS.writeFile (outputDir / "dep_graph.html") depGraphHtmlStr
  IO.println s!"  Generated dep_graph.html"

  IO.println s!"Multi-page site generated at {outputDir}"

end Runway
