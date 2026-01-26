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

/-- Render sidebar navigation for chapters -/
def renderSidebar (chapters : Array ChapterInfo) (currentSlug : Option String) (toRoot : String) : Html :=
  let homeClass := if currentSlug.isNone then "active" else ""
  let homeItem := .tag "li" #[("class", homeClass)] (
    .tag "a" #[("href", s!"{toRoot}index.html")] (Html.text true "Blueprint Home")
  )

  -- Dependency graph link
  let graphClass := if currentSlug == some "dep_graph" then "active" else ""
  let graphItem := .tag "li" #[("class", graphClass)] (
    .tag "a" #[("href", s!"{toRoot}dep_graph.html")] (Html.text true "Dependency Graph")
  )

  let chapterItems := chapters.map fun chapter =>
    let isActive := currentSlug == some chapter.slug
    let itemClass := if isActive then "active" else ""
    let href := s!"{toRoot}{chapter.slug}.html"
    let chapterPrefix := if chapter.isAppendix then "Appendix" else s!"{chapter.number}."
    .tag "li" #[("class", itemClass)] (
      .tag "a" #[("href", href)] (Html.text true s!"{chapterPrefix} {chapter.title}")
    )

  .tag "nav" #[("class", "toc")] (
    .tag "ul" #[("class", "sub-toc-0")] (
      homeItem ++ graphItem ++ .seq chapterItems
    )
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

  -- MathJax configuration script
  let mathjaxConfig := .tag "script" #[] (Html.text false r#"
    MathJax = {
      tex: {
        inlineMath: [['$', '$'], ['\\(', '\\)']],
        displayMath: [['$$', '$$'], ['\\[', '\\]']],
        processEscapes: true
      },
      options: {
        skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
      }
    };
  "#)

  return .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true config.title) ++
      -- Local CSS
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
            .tag "a" #[("class", "brand-logo left"), ("href", s!"{toRoot}index.html")] (
              Html.text true config.title
            ) ++
            .tag "ul" #[("class", "nav-list right")] (
              (match config.githubUrl with
               | some url => .tag "li" #[] (
                   .tag "a" #[("href", url), ("target", "_blank")] (Html.text true "GitHub")
                 )
               | none => Html.empty) ++
              (match config.docgen4Url with
               | some url => .tag "li" #[] (
                   .tag "a" #[("href", url), ("target", "_blank")] (Html.text true "API Docs")
                 )
               | none => Html.empty)
            )
          )
        )
      ) ++
      -- Main wrapper with sidebar and content
      divClass "wrapper" (
        -- Sidebar toggle button (for mobile)
        .tag "span" #[("id", "toc-toggle")] (Html.text true "☰") ++
        -- Table of contents sidebar (default single-page version)
        .tag "nav" #[("class", "toc")] (
          .tag "ul" #[("class", "sub-toc-0")] (
            .tag "li" #[("class", "active")] (
              .tag "a" #[("href", s!"{toRoot}index.html")] (Html.text true "Blueprint Home")
            )
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
def primaryTemplateWithSidebar (chapters : Array ChapterInfo) (currentSlug : Option String) : Template := fun content => do
  let config ← Render.getConfig
  let toRoot ← Render.pathToRoot

  -- MathJax configuration script
  let mathjaxConfig := .tag "script" #[] (Html.text false r#"
    MathJax = {
      tex: {
        inlineMath: [['$', '$'], ['\\(', '\\)']],
        displayMath: [['$$', '$$'], ['\\[', '\\]']],
        processEscapes: true
      },
      options: {
        skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
      }
    };
  "#)

  -- Build sidebar
  let sidebar := renderSidebar chapters currentSlug toRoot

  -- Build prev/next nav for chapter pages
  let prevNextNav := match currentSlug with
    | some slug => renderPrevNextNav chapters slug toRoot
    | none => Html.empty

  return .tag "html" #[("lang", "en")] (
    .tag "head" #[] (
      .tag "meta" #[("charset", "UTF-8")] Html.empty ++
      .tag "meta" #[("name", "viewport"), ("content", "width=device-width, initial-scale=1")] Html.empty ++
      .tag "title" #[] (Html.text true config.title) ++
      -- Local CSS
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
            .tag "a" #[("class", "brand-logo left"), ("href", s!"{toRoot}index.html")] (
              Html.text true config.title
            ) ++
            .tag "ul" #[("class", "nav-list right")] (
              (match config.githubUrl with
               | some url => .tag "li" #[] (
                   .tag "a" #[("href", url), ("target", "_blank")] (Html.text true "GitHub")
                 )
               | none => Html.empty) ++
              (match config.docgen4Url with
               | some url => .tag "li" #[] (
                   .tag "a" #[("href", url), ("target", "_blank")] (Html.text true "API Docs")
                 )
               | none => Html.empty)
            )
          )
        )
      ) ++
      -- Main wrapper with sidebar and content
      divClass "wrapper" (
        -- Sidebar toggle button (for mobile)
        .tag "span" #[("id", "toc-toggle")] (Html.text true "☰") ++
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
def generateSite (theme : Theme) (site : BlueprintSite) (outputDir : System.FilePath) : IO Unit := do
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

  -- Generate dedicated dependency graph page with rich modals
  let (modalsHtml, _) ← renderAllModals site.nodes |>.run ctx
  let depGraphPage := DepGraph.fullPageGraph site.depGraphSvg site.depGraphJson (some modalsHtml) site.config.title
  let depGraphHtmlStr := Html.doctype ++ "\n" ++ depGraphPage.asString
  IO.FS.writeFile (outputDir / "dep_graph.html") depGraphHtmlStr

  IO.println s!"Site generated at {outputDir}"

/-- Build a lookup map from node label to NodeInfo -/
def buildNodeLookup (nodes : Array NodeInfo) : Std.HashMap String NodeInfo :=
  nodes.foldl (init := {}) fun acc node =>
    acc.insert node.label node

/-- Replace placeholder divs with rendered node HTML.
    Finds `<div class="lean-node-placeholder" data-node="X"></div>` and replaces with rendered node.
    Uses splitOn for reliable parsing.
-/
def replacePlaceholders (proseHtml : String) (nodeLookup : Std.HashMap String NodeInfo)
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

/-- Render a chapter page content -/
def renderChapterContent (chapter : ChapterInfo) (allNodes : Array NodeInfo) : RenderM Html := do
  -- Build node lookup for placeholder resolution
  let nodeLookup := buildNodeLookup allNodes

  -- Chapter title
  let titlePrefix := if chapter.isAppendix then "Appendix" else s!"Chapter {chapter.number}"
  let chapterTitle := .tag "h1" #[("class", "chapter-title")] (
    Html.text true s!"{titlePrefix}: {chapter.title}"
  )

  -- Prose content with placeholders resolved to actual rendered nodes
  let resolvedProseHtml ← replacePlaceholders chapter.proseHtml nodeLookup
  let proseHtml := Html.text false resolvedProseHtml

  -- Render sections
  let mut sectionHtmls : Array Html := #[]
  for sec in chapter.sections do
    let sectionTitle := match sec.number with
      | some n => .tag "h2" #[("class", "section-title")] (Html.text true s!"{chapter.number}.{n} {sec.title}")
      | none => .tag "h2" #[("class", "section-title")] (Html.text true sec.title)

    -- Section prose with placeholders resolved
    let resolvedSectionProseHtml ← replacePlaceholders sec.proseHtml nodeLookup
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

  -- Title section
  let titleSection := divClass "index-header" (
    .tag "h1" #[] (Html.text true config.title) ++
    (match config.githubUrl with
     | some url => htmlLink url (Html.text true "GitHub") (some "github-link")
     | none => Html.empty) ++
    (match config.docgen4Url with
     | some url => htmlLink url (Html.text true "Documentation") (some "docs-link")
     | none => Html.empty)
  )

  -- Progress section
  let progress := renderProgress site

  -- Chapter list
  let chapterList := divClass "chapter-list" (
    .tag "h2" #[] (Html.text true "Chapters") ++
    .tag "ol" #[("class", "chapter-index")] (
      .seq (site.chapters.map fun chapter =>
        let chapterPrefix := if chapter.isAppendix then "Appendix" else ""
        .tag "li" #[] (
          .tag "a" #[("href", s!"{chapter.slug}.html")] (
            Html.text true (if chapterPrefix.isEmpty then chapter.title else s!"{chapterPrefix}: {chapter.title}")
          )
        )
      )
    )
  )

  return divClass "index-page" (
    titleSection ++
    progress ++
    chapterList
  )

/-- Generate multi-page site with chapter-based navigation -/
def generateMultiPageSite (_theme : Theme) (site : BlueprintSite) (outputDir : System.FilePath) : IO Unit := do
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
  let templateWithSidebar := DefaultTheme.primaryTemplateWithSidebar site.chapters none
  let (indexHtml, _) ← templateWithSidebar indexContent |>.run ctx
  let indexHtmlStr := Html.doctype ++ "\n" ++ indexHtml.asString
  IO.FS.writeFile (outputDir / "index.html") indexHtmlStr
  IO.println s!"  Generated index.html"

  -- Generate chapter pages
  for chap in site.chapters do
    let (chapterContent, _) ← renderChapterContent chap site.nodes |>.run ctx
    let chapterTemplate := DefaultTheme.primaryTemplateWithSidebar site.chapters (some chap.slug)
    let (chapterHtml, _) ← chapterTemplate chapterContent |>.run ctx
    let chapterHtmlStr := Html.doctype ++ "\n" ++ chapterHtml.asString
    IO.FS.writeFile (outputDir / s!"{chap.slug}.html") chapterHtmlStr
    IO.println s!"  Generated {chap.slug}.html"

  -- Generate dedicated dependency graph page with rich modals
  let (modalsHtml, _) ← renderAllModals site.nodes |>.run ctx
  let depGraphPage := DepGraph.fullPageGraph site.depGraphSvg site.depGraphJson (some modalsHtml) site.config.title
  let depGraphHtmlStr := Html.doctype ++ "\n" ++ depGraphPage.asString
  IO.FS.writeFile (outputDir / "dep_graph.html") depGraphHtmlStr
  IO.println s!"  Generated dep_graph.html"

  IO.println s!"Multi-page site generated at {outputDir}"

end Runway
