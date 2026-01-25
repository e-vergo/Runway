/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Static Assets

Embeds CSS and JavaScript as string literals for distribution.
These assets are written to the output directory during site generation.

Note: Lean 4 does not have `include_str!`, so we embed assets as string literals.
For large assets, consider loading from files at runtime.
-/

namespace Runway.Assets

/-- Main Runway CSS stylesheet.
    This is derived from Theme.defaultCss but kept here for the CLI to use. -/
def runwayCss : String := r#"
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

/* Verso Code Styles (for SubVerso highlighting) */
.hl.lean {
  font-family: var(--bp-font-mono);
  white-space: pre-wrap;
}

.hl.lean .keyword { color: #af00db; }
.hl.lean .name { color: #001080; }
.hl.lean .const { color: #0070c1; }
.hl.lean .string { color: #a31515; }
.hl.lean .number { color: #098658; }
.hl.lean .comment { color: #008000; }
.hl.lean .docstring { color: #008000; font-style: italic; }

@media (prefers-color-scheme: dark) {
  .hl.lean .keyword { color: #c586c0; }
  .hl.lean .name { color: #9cdcfe; }
  .hl.lean .const { color: #4ec9b0; }
  .hl.lean .string { color: #ce9178; }
  .hl.lean .number { color: #b5cea8; }
  .hl.lean .comment { color: #6a9955; }
  .hl.lean .docstring { color: #6a9955; }
}

/* Hover tooltip styles */
.hl.lean [data-hover] {
  cursor: help;
  border-bottom: 1px dotted var(--bp-muted);
}

.verso-hover-tooltip {
  position: absolute;
  z-index: 1000;
  max-width: 500px;
  padding: 0.75rem;
  background: var(--bp-bg);
  border: 1px solid var(--bp-border);
  border-radius: 6px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
  font-size: 0.875rem;
  pointer-events: none;
  opacity: 0;
  transition: opacity 0.15s ease;
}

.verso-hover-tooltip.visible {
  opacity: 1;
}

.verso-hover-tooltip .tooltip-type {
  font-family: var(--bp-font-mono);
  color: var(--bp-text-muted);
  margin-bottom: 0.5rem;
}

.verso-hover-tooltip .tooltip-docs {
  color: var(--bp-text);
}

/* Math rendering */
.math.inline { }
.math.display {
  overflow-x: auto;
  padding: 1rem 0;
}
"#

/-- Verso code hover tooltips and binding highlighting JavaScript -/
def versoCodeJs : String := r#"
/**
 * Verso Code Interactivity
 * Provides hover tooltips for Lean code and binding highlighting.
 */
(function() {
  'use strict';

  document.addEventListener('DOMContentLoaded', init);

  let tooltip = null;
  let activeElement = null;
  let hoverTimeout = null;
  const HOVER_DELAY = 300;

  function init() {
    createTooltip();
    setupHoverListeners();
    setupBindingHighlight();
  }

  function createTooltip() {
    tooltip = document.createElement('div');
    tooltip.className = 'verso-hover-tooltip';
    tooltip.setAttribute('role', 'tooltip');
    document.body.appendChild(tooltip);
  }

  function setupHoverListeners() {
    // Find all elements with hover data
    const hoverElements = document.querySelectorAll('[data-hover]');

    hoverElements.forEach(function(el) {
      el.addEventListener('mouseenter', function(e) {
        clearTimeout(hoverTimeout);
        hoverTimeout = setTimeout(function() {
          showTooltip(el, e);
        }, HOVER_DELAY);
      });

      el.addEventListener('mouseleave', function() {
        clearTimeout(hoverTimeout);
        hideTooltip();
      });

      el.addEventListener('mousemove', function(e) {
        if (tooltip.classList.contains('visible')) {
          positionTooltip(e);
        }
      });
    });
  }

  function showTooltip(el, event) {
    const hoverData = el.getAttribute('data-hover');
    if (!hoverData) return;

    try {
      const data = JSON.parse(hoverData);
      renderTooltipContent(data);
      positionTooltip(event);
      tooltip.classList.add('visible');
      activeElement = el;
    } catch (e) {
      // If not JSON, treat as plain text type signature
      tooltip.innerHTML = '<div class="tooltip-type">' + escapeHtml(hoverData) + '</div>';
      positionTooltip(event);
      tooltip.classList.add('visible');
      activeElement = el;
    }
  }

  function hideTooltip() {
    tooltip.classList.remove('visible');
    activeElement = null;
  }

  function renderTooltipContent(data) {
    let html = '';

    // Type signature
    if (data.type) {
      html += '<div class="tooltip-type">' + escapeHtml(data.type) + '</div>';
    }

    // Documentation
    if (data.doc) {
      html += '<div class="tooltip-docs">' + escapeHtml(data.doc) + '</div>';
    }

    // Source location
    if (data.source) {
      html += '<div class="tooltip-source">' + escapeHtml(data.source) + '</div>';
    }

    tooltip.innerHTML = html || '<div class="tooltip-type">No information available</div>';
  }

  function positionTooltip(event) {
    const margin = 10;
    const viewportWidth = window.innerWidth;
    const viewportHeight = window.innerHeight;

    let x = event.clientX + margin;
    let y = event.clientY + margin;

    // Measure tooltip
    tooltip.style.left = '0';
    tooltip.style.top = '0';
    const rect = tooltip.getBoundingClientRect();

    // Adjust for viewport boundaries
    if (x + rect.width > viewportWidth - margin) {
      x = event.clientX - rect.width - margin;
    }
    if (y + rect.height > viewportHeight - margin) {
      y = event.clientY - rect.height - margin;
    }

    // Ensure not off-screen
    x = Math.max(margin, x);
    y = Math.max(margin, y);

    tooltip.style.left = x + 'px';
    tooltip.style.top = y + 'px';
  }

  function setupBindingHighlight() {
    // Find all elements with binding IDs
    const bindingElements = document.querySelectorAll('[data-binding-id]');

    bindingElements.forEach(function(el) {
      el.addEventListener('mouseenter', function() {
        highlightBinding(el.getAttribute('data-binding-id'), true);
      });

      el.addEventListener('mouseleave', function() {
        highlightBinding(el.getAttribute('data-binding-id'), false);
      });
    });
  }

  function highlightBinding(bindingId, highlight) {
    const elements = document.querySelectorAll('[data-binding-id="' + bindingId + '"]');
    elements.forEach(function(el) {
      if (highlight) {
        el.classList.add('binding-highlight');
      } else {
        el.classList.remove('binding-highlight');
      }
    });
  }

  function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

})();
"#

/-- Dependency graph pan/zoom JavaScript -/
def depGraphJs : String := r#"
/**
 * Dependency Graph Interactivity
 * Provides pan, zoom, and navigation for the embedded SVG dependency graph.
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

  // DOM elements
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

    // Touch support
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

/-- Combined runway.js with both dep-graph and verso-code functionality -/
def runwayJs : String := versoCodeJs ++ "\n" ++ depGraphJs

/-- Write all assets to the output directory -/
def writeAssets (outputDir : System.FilePath) : IO Unit := do
  IO.FS.writeFile (outputDir / "runway.css") runwayCss
  IO.FS.writeFile (outputDir / "runway.js") runwayJs
  IO.FS.writeFile (outputDir / "verso-code.js") versoCodeJs
  IO.FS.writeFile (outputDir / "dep-graph.js") depGraphJs

end Runway.Assets
