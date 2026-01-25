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
import Runway.Assets

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

/-- Default Blueprint CSS styles -/
def defaultCss : String := r#"
/* Blueprint Default Theme */
:root {
  --bp-primary: #2563eb;
  --bp-success: #16a34a;
  --bp-warning: #ca8a04;
  --bp-danger: #dc2626;
  --bp-muted: #6b7280;
  --bp-bg: #ffffff;
  --bp-bg-alt: #f9fafb;
  --bp-border: #e5e7eb;
  --bp-text: #1f2937;
  --bp-text-muted: #6b7280;
  --bp-font-sans: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  --bp-font-mono: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Monaco, Consolas, monospace;
  --bp-max-width: 1200px;
  --bp-spacing: 1.5rem;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bp-bg: #111827;
    --bp-bg-alt: #1f2937;
    --bp-border: #374151;
    --bp-text: #f9fafb;
    --bp-text-muted: #9ca3af;
  }
}

* { box-sizing: border-box; }

body {
  font-family: var(--bp-font-sans);
  line-height: 1.6;
  color: var(--bp-text);
  background: var(--bp-bg);
  margin: 0;
  padding: var(--bp-spacing);
}

.blueprint-container {
  max-width: var(--bp-max-width);
  margin: 0 auto;
}

/* Navigation */
.blueprint-nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 0;
  border-bottom: 1px solid var(--bp-border);
  margin-bottom: 2rem;
}

.blueprint-nav h1 {
  margin: 0;
  font-size: 1.5rem;
}

.nav-links {
  display: flex;
  gap: 1rem;
}

.nav-links a {
  color: var(--bp-primary);
  text-decoration: none;
}

.nav-links a:hover {
  text-decoration: underline;
}

/* Progress Section */
.progress-section {
  margin: 2rem 0;
  padding: 1.5rem;
  background: var(--bp-bg-alt);
  border-radius: 8px;
}

.progress-bar {
  height: 24px;
  background: var(--bp-border);
  border-radius: 12px;
  overflow: hidden;
  margin: 1rem 0;
}

.progress-fill {
  height: 100%;
  background: linear-gradient(90deg, var(--bp-success), var(--bp-primary));
  transition: width 0.3s ease;
}

.progress-stats {
  display: flex;
  flex-wrap: wrap;
  gap: 1rem;
  margin-top: 1rem;
}

.stat {
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  font-size: 0.875rem;
}

.stat.proved { background: rgba(22, 163, 74, 0.1); color: var(--bp-success); }
.stat.mathlib { background: rgba(37, 99, 235, 0.1); color: var(--bp-primary); }
.stat.stated { background: rgba(202, 138, 4, 0.1); color: var(--bp-warning); }
.stat.not-ready { background: rgba(220, 38, 38, 0.1); color: var(--bp-danger); }
.stat.total { background: var(--bp-border); }

/* Node Styles */
.node {
  border: 1px solid var(--bp-border);
  border-radius: 8px;
  margin: 1.5rem 0;
  overflow: hidden;
}

.node-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 1rem;
  background: var(--bp-bg-alt);
  border-bottom: 1px solid var(--bp-border);
}

.node-env {
  font-weight: 600;
  text-transform: capitalize;
}

.node-title {
  flex: 1;
}

.node-status {
  width: 12px;
  height: 12px;
  border-radius: 50%;
}

.status-proved .node-status, .node-proved .node-status { background: var(--bp-success); }
.status-mathlib-ok .node-status, .node-mathlib-ok .node-status { background: var(--bp-primary); }
.status-stated .node-status, .node-stated .node-status { background: var(--bp-warning); }
.status-not-ready .node-status, .node-not-ready .node-status { background: var(--bp-danger); }

.node-content {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem;
  padding: 1rem;
}

@media (max-width: 768px) {
  .node-content {
    grid-template-columns: 1fr;
  }
}

.node-statement, .node-proof {
  padding: 1rem;
  background: var(--bp-bg-alt);
  border-radius: 4px;
}

.node-statement h4, .node-proof h4 {
  margin: 0 0 0.75rem 0;
  font-size: 0.875rem;
  color: var(--bp-text-muted);
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.node-footer {
  padding: 0.75rem 1rem;
  border-top: 1px solid var(--bp-border);
  font-size: 0.875rem;
  color: var(--bp-text-muted);
}

.node-decls, .node-deps {
  display: inline;
}

.node-decls + .node-deps::before {
  content: " | ";
  margin: 0 0.5rem;
}

.decl-link, .dep-link {
  color: var(--bp-primary);
  text-decoration: none;
}

.decl-link:hover, .dep-link:hover {
  text-decoration: underline;
}

/* Index Page */
.index-header {
  text-align: center;
  margin-bottom: 2rem;
}

.index-header h1 {
  margin-bottom: 1rem;
}

.github-link, .docs-link {
  margin: 0 0.5rem;
  color: var(--bp-primary);
}

/* Graph Section */
.graph-section {
  margin: 2rem 0;
}

.dep-graph-container {
  position: relative;
  width: 100%;
  border: 1px solid var(--bp-border);
  border-radius: 8px;
  background: var(--bp-bg-alt);
  overflow: hidden;
}

.dep-graph-toolbar {
  position: absolute;
  top: 0.5rem;
  right: 0.5rem;
  z-index: 10;
  display: flex;
  gap: 0.25rem;
  background: var(--bp-bg);
  border: 1px solid var(--bp-border);
  border-radius: 4px;
  padding: 0.25rem;
}

.dep-graph-toolbar button {
  padding: 0.25rem 0.5rem;
  border: 1px solid var(--bp-border);
  border-radius: 3px;
  background: var(--bp-bg);
  color: var(--bp-text);
  cursor: pointer;
  font-size: 0.875rem;
  line-height: 1;
  transition: background 0.15s ease;
}

.dep-graph-toolbar button:hover {
  background: var(--bp-bg-alt);
}

.dep-graph-toolbar button:active {
  background: var(--bp-border);
}

.dep-graph-viewport {
  width: 100%;
  height: 500px;
  overflow: hidden;
  cursor: grab;
}

.dep-graph-viewport:active {
  cursor: grabbing;
}

.dep-graph-svg {
  transform-origin: 0 0;
  transition: transform 0.05s ease-out;
}

.dep-graph-svg svg {
  display: block;
}

.dep-graph-placeholder {
  display: flex;
  align-items: center;
  justify-content: center;
  height: 200px;
  color: var(--bp-text-muted);
  font-style: italic;
}

/* Node highlight animation */
.node-highlight {
  animation: node-flash 2s ease-out;
}

@keyframes node-flash {
  0%, 100% { box-shadow: none; }
  20%, 80% { box-shadow: 0 0 0 4px var(--bp-primary); }
}

/* Node Lists */
.node-lists {
  margin: 2rem 0;
}

.node-list h3 {
  margin-bottom: 1rem;
}

.node-index {
  list-style: none;
  padding: 0;
  margin: 0;
}

.node-index li {
  padding: 0.5rem 0;
  border-bottom: 1px solid var(--bp-border);
}

.node-index a {
  color: var(--bp-text);
  text-decoration: none;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.node-index a:hover {
  color: var(--bp-primary);
}

/* Lean Code Highlighting */
pre.lean-code {
  font-family: var(--bp-font-mono);
  font-size: 0.875rem;
  padding: 1rem;
  overflow-x: auto;
  background: var(--bp-bg);
  border-radius: 4px;
  border: 1px solid var(--bp-border);
}

/* Math rendering */
.math.inline { }
.math.display {
  overflow-x: auto;
  padding: 1rem 0;
}
"#

/-- Default JavaScript for interactivity -/
def defaultJs : String := r#"
/**
 * Blueprint Interactivity - includes dependency graph controls
 */
(function() {
  'use strict';

  document.addEventListener('DOMContentLoaded', init);

  // Graph state
  let scale = 1;
  let translateX = 0;
  let translateY = 0;
  let isDragging = false;
  let startX = 0;
  let startY = 0;
  let graphData = null;
  let container = null;
  let viewport = null;
  let svgWrapper = null;
  let svg = null;

  function init() {
    // Load graph data if present
    const dataEl = document.getElementById('dep-graph-data');
    if (dataEl) {
      try {
        graphData = JSON.parse(dataEl.textContent);
      } catch (e) {
        console.warn('Failed to parse graph data:', e);
      }
    }

    // Get DOM elements
    container = document.querySelector('.dep-graph-container');
    viewport = document.getElementById('dep-graph-viewport');
    svgWrapper = document.getElementById('dep-graph');

    if (!container || !svgWrapper) return;

    svg = svgWrapper.querySelector('svg');
    if (!svg) return;

    setupZoomControls();
    setupPanZoom();
    setupNodeInteraction();
    setTimeout(fitToWindow, 100);
  }

  function setupZoomControls() {
    const zoomIn = document.getElementById('graph-zoom-in');
    const zoomOut = document.getElementById('graph-zoom-out');
    const reset = document.getElementById('graph-reset');
    const fit = document.getElementById('graph-fit');

    if (zoomIn) zoomIn.addEventListener('click', function() { zoom(1.2); });
    if (zoomOut) zoomOut.addEventListener('click', function() { zoom(0.8); });
    if (reset) reset.addEventListener('click', function() {
      scale = 1; translateX = 0; translateY = 0; updateTransform();
    });
    if (fit) fit.addEventListener('click', fitToWindow);
  }

  function setupPanZoom() {
    if (!viewport) return;

    viewport.addEventListener('wheel', function(e) {
      e.preventDefault();
      const delta = e.deltaY > 0 ? 0.9 : 1.1;
      const rect = viewport.getBoundingClientRect();
      zoomAt(delta, e.clientX - rect.left, e.clientY - rect.top);
    }, { passive: false });

    viewport.addEventListener('mousedown', function(e) {
      if (e.button !== 0) return;
      isDragging = true;
      startX = e.clientX - translateX;
      startY = e.clientY - translateY;
      viewport.style.cursor = 'grabbing';
    });

    document.addEventListener('mousemove', function(e) {
      if (!isDragging) return;
      translateX = e.clientX - startX;
      translateY = e.clientY - startY;
      updateTransform();
    });

    document.addEventListener('mouseup', function() {
      if (isDragging) {
        isDragging = false;
        if (viewport) viewport.style.cursor = 'grab';
      }
    });

    let touchStartDist = 0;
    let touchStartScale = 1;

    viewport.addEventListener('touchstart', function(e) {
      if (e.touches.length === 1) {
        isDragging = true;
        startX = e.touches[0].clientX - translateX;
        startY = e.touches[0].clientY - translateY;
      } else if (e.touches.length === 2) {
        isDragging = false;
        touchStartDist = getTouchDistance(e.touches);
        touchStartScale = scale;
      }
    }, { passive: true });

    viewport.addEventListener('touchmove', function(e) {
      if (e.touches.length === 1 && isDragging) {
        e.preventDefault();
        translateX = e.touches[0].clientX - startX;
        translateY = e.touches[0].clientY - startY;
        updateTransform();
      } else if (e.touches.length === 2) {
        e.preventDefault();
        const dist = getTouchDistance(e.touches);
        scale = Math.max(0.1, Math.min(10, touchStartScale * (dist / touchStartDist)));
        updateTransform();
      }
    }, { passive: false });

    viewport.addEventListener('touchend', function() { isDragging = false; });
    viewport.style.cursor = 'grab';
  }

  function setupNodeInteraction() {
    if (!svg || !graphData) return;

    const nodeLinks = svg.querySelectorAll('a[href]');
    nodeLinks.forEach(function(link) {
      const href = link.getAttribute('href');
      if (!href || !href.startsWith('#')) return;

      const nodeId = href.substring(1);
      const node = graphData.nodes ? graphData.nodes.find(function(n) {
        return n.id === nodeId;
      }) : null;

      link.addEventListener('mouseenter', function() { highlightDependencies(nodeId, true); });
      link.addEventListener('mouseleave', function() { highlightDependencies(nodeId, false); });
      link.addEventListener('click', function(e) { e.preventDefault(); navigateToNode(nodeId); });

      if (node) {
        const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
        title.textContent = node.label + ' (' + node.status + ')';
        link.insertBefore(title, link.firstChild);
      }
    });
  }

  function highlightDependencies(nodeId, highlight) {
    if (!svg || !graphData || !graphData.edges) return;

    const relatedNodes = new Set();
    graphData.edges.forEach(function(e) {
      if (e.to === nodeId) relatedNodes.add(e.from);
      if (e.from === nodeId) relatedNodes.add(e.to);
    });

    svg.querySelectorAll('a[href]').forEach(function(link) {
      const href = link.getAttribute('href');
      if (!href) return;
      const id = href.substring(1);

      if (highlight) {
        if (id === nodeId) {
          link.style.opacity = '1';
          link.style.filter = 'drop-shadow(0 0 4px var(--bp-primary, #2563eb))';
        } else if (relatedNodes.has(id)) {
          link.style.opacity = '1';
          link.style.filter = 'drop-shadow(0 0 2px var(--bp-muted, #6b7280))';
        } else {
          link.style.opacity = '0.3';
          link.style.filter = '';
        }
      } else {
        link.style.opacity = '';
        link.style.filter = '';
      }
    });

    const paths = svg.querySelectorAll('.edges path');
    paths.forEach(function(path) {
      if (highlight) {
        const d = path.getAttribute('d') || '';
        const isRelated = graphData.edges.some(function(edge) {
          return edge.from === nodeId || edge.to === nodeId;
        });
        path.style.opacity = isRelated ? '1' : '0.2';
        path.style.strokeWidth = isRelated ? '2.5' : '';
      } else {
        path.style.opacity = '';
        path.style.strokeWidth = '';
      }
    });
  }

  function navigateToNode(nodeId) {
    const element = document.getElementById(nodeId);
    if (element) {
      element.scrollIntoView({ behavior: 'smooth', block: 'center' });
      element.classList.add('node-highlight');
      setTimeout(function() { element.classList.remove('node-highlight'); }, 2000);
    }
  }

  function zoom(factor) {
    scale = Math.max(0.1, Math.min(10, scale * factor));
    updateTransform();
  }

  function zoomAt(factor, x, y) {
    const oldScale = scale;
    scale = Math.max(0.1, Math.min(10, scale * factor));
    translateX = x - (x - translateX) * (scale / oldScale);
    translateY = y - (y - translateY) * (scale / oldScale);
    updateTransform();
  }

  function fitToWindow() {
    if (!viewport || !svg) return;
    const viewportRect = viewport.getBoundingClientRect();
    const svgWidth = parseFloat(svg.getAttribute('width')) || svg.getBBox().width;
    const svgHeight = parseFloat(svg.getAttribute('height')) || svg.getBBox().height;
    if (svgWidth === 0 || svgHeight === 0) return;

    const padding = 20;
    scale = Math.min((viewportRect.width - padding * 2) / svgWidth,
                     (viewportRect.height - padding * 2) / svgHeight, 1);
    translateX = (viewportRect.width - svgWidth * scale) / 2;
    translateY = (viewportRect.height - svgHeight * scale) / 2;
    updateTransform();
  }

  function updateTransform() {
    if (!svgWrapper) return;
    svgWrapper.style.transform = 'translate(' + translateX + 'px, ' + translateY + 'px) scale(' + scale + ')';
  }

  function getTouchDistance(touches) {
    const dx = touches[0].clientX - touches[1].clientX;
    const dy = touches[0].clientY - touches[1].clientY;
    return Math.sqrt(dx * dx + dy * dy);
  }
})();
"#

/-- Render sidebar navigation for chapters -/
def renderSidebar (chapters : Array ChapterInfo) (currentSlug : Option String) (toRoot : String) : Html :=
  let homeClass := if currentSlug.isNone then "active" else ""
  let homeItem := .tag "li" #[("class", homeClass)] (
    .tag "a" #[("href", s!"{toRoot}index.html")] (Html.text true "Blueprint Home")
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
      homeItem ++ .seq chapterItems
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
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}runway.css")] Html.empty ++
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
      .tag "script" #[("src", s!"{toRoot}runway.js")] Html.empty
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
      .tag "link" #[("rel", "stylesheet"), ("href", s!"{toRoot}runway.css")] Html.empty ++
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
      .tag "script" #[("src", s!"{toRoot}runway.js")] Html.empty
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
  cssFiles := #[("runway.css", Assets.blueprintCss)]
  jsFiles := #[("runway.js", Assets.runwayJs, false)]

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

/-- Write theme assets to output directory -/
def Theme.writeAssets (theme : Theme) (outputDir : System.FilePath) : IO Unit := do
  -- Write CSS files
  for (filename, content) in theme.cssFiles do
    IO.FS.writeFile (outputDir / filename) content

  -- Write JS files
  for (filename, content, _) in theme.jsFiles do
    IO.FS.writeFile (outputDir / filename) content

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

  -- Write theme assets
  theme.writeAssets outputDir

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

  -- Dependency graph section
  let graphSection := DepGraph.graphSection site.depGraphSvg site.depGraphJson

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
    graphSection ++
    chapterList
  )

/-- Generate multi-page site with chapter-based navigation -/
def generateMultiPageSite (theme : Theme) (site : BlueprintSite) (outputDir : System.FilePath) : IO Unit := do
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

  -- Write theme assets
  theme.writeAssets outputDir

  IO.println s!"Multi-page site generated at {outputDir}"

end Runway
