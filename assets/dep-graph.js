/**
 * Dependency Graph Interactivity
 *
 * Provides pan, zoom, and navigation for the embedded SVG dependency graph.
 * Works with the graph data exported by Dress.
 */
(function() {
  'use strict';

  // Wait for DOM
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

    // Setup controls
    setupZoomControls();
    setupPanZoom();
    setupNodeInteraction();

    // Initial fit
    setTimeout(fitToWindow, 100);
  }

  function setupZoomControls() {
    const zoomIn = document.getElementById('graph-zoom-in');
    const zoomOut = document.getElementById('graph-zoom-out');
    const reset = document.getElementById('graph-reset');
    const fit = document.getElementById('graph-fit');

    if (zoomIn) {
      zoomIn.addEventListener('click', function() {
        zoom(1.2);
      });
    }

    if (zoomOut) {
      zoomOut.addEventListener('click', function() {
        zoom(0.8);
      });
    }

    if (reset) {
      reset.addEventListener('click', function() {
        scale = 1;
        translateX = 0;
        translateY = 0;
        updateTransform();
      });
    }

    if (fit) {
      fit.addEventListener('click', fitToWindow);
    }
  }

  function setupPanZoom() {
    if (!viewport) return;

    // Mouse wheel zoom
    viewport.addEventListener('wheel', function(e) {
      e.preventDefault();
      const delta = e.deltaY > 0 ? 0.9 : 1.1;

      // Zoom toward cursor position
      const rect = viewport.getBoundingClientRect();
      const x = e.clientX - rect.left;
      const y = e.clientY - rect.top;

      zoomAt(delta, x, y);
    }, { passive: false });

    // Pan with mouse drag
    viewport.addEventListener('mousedown', function(e) {
      if (e.button !== 0) return; // Left click only
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
        scale = touchStartScale * (dist / touchStartDist);
        scale = Math.max(0.1, Math.min(10, scale));
        updateTransform();
      }
    }, { passive: false });

    viewport.addEventListener('touchend', function() {
      isDragging = false;
    });

    // Set initial cursor
    viewport.style.cursor = 'grab';
  }

  function setupNodeInteraction() {
    if (!svg || !graphData) return;

    // Find all node rectangles/links
    const nodeLinks = svg.querySelectorAll('a[href]');

    nodeLinks.forEach(function(link) {
      const href = link.getAttribute('href');
      if (!href || !href.startsWith('#')) return;

      const nodeId = href.substring(1);
      const node = graphData.nodes ? graphData.nodes.find(function(n) {
        return n.id === nodeId;
      }) : null;

      // Hover effects
      link.addEventListener('mouseenter', function() {
        highlightDependencies(nodeId, true);
      });

      link.addEventListener('mouseleave', function() {
        highlightDependencies(nodeId, false);
      });

      // Click to navigate
      link.addEventListener('click', function(e) {
        e.preventDefault();
        navigateToNode(nodeId);
      });

      // Add tooltip with node info
      if (node) {
        const title = document.createElementNS('http://www.w3.org/2000/svg', 'title');
        title.textContent = node.label + ' (' + node.status + ')';
        link.insertBefore(title, link.firstChild);
      }
    });
  }

  function highlightDependencies(nodeId, highlight) {
    if (!svg || !graphData || !graphData.edges) return;

    // Find edges involving this node
    const incomingEdges = graphData.edges.filter(function(e) {
      return e.to === nodeId;
    });
    const outgoingEdges = graphData.edges.filter(function(e) {
      return e.from === nodeId;
    });

    // Get related node IDs
    const relatedNodes = new Set();
    incomingEdges.forEach(function(e) { relatedNodes.add(e.from); });
    outgoingEdges.forEach(function(e) { relatedNodes.add(e.to); });

    // Apply highlighting
    const allLinks = svg.querySelectorAll('a[href]');
    allLinks.forEach(function(link) {
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

    // Highlight edges
    const paths = svg.querySelectorAll('path');
    paths.forEach(function(path, index) {
      if (!graphData.edges[index]) return;
      const edge = graphData.edges[index];

      if (highlight) {
        const isRelated = edge.from === nodeId || edge.to === nodeId;
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

      // Flash highlight
      element.classList.add('node-highlight');
      setTimeout(function() {
        element.classList.remove('node-highlight');
      }, 2000);
    }
  }

  function zoom(factor) {
    scale *= factor;
    scale = Math.max(0.1, Math.min(10, scale));
    updateTransform();
  }

  function zoomAt(factor, x, y) {
    const oldScale = scale;
    scale *= factor;
    scale = Math.max(0.1, Math.min(10, scale));

    // Adjust translation to zoom toward point
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
    const scaleX = (viewportRect.width - padding * 2) / svgWidth;
    const scaleY = (viewportRect.height - padding * 2) / svgHeight;

    scale = Math.min(scaleX, scaleY, 1); // Don't scale up beyond 1

    // Center the graph
    translateX = (viewportRect.width - svgWidth * scale) / 2;
    translateY = (viewportRect.height - svgHeight * scale) / 2;

    updateTransform();
  }

  function updateTransform() {
    if (!svgWrapper) return;
    svgWrapper.style.transform =
      'translate(' + translateX + 'px, ' + translateY + 'px) scale(' + scale + ')';
  }

  function getTouchDistance(touches) {
    const dx = touches[0].clientX - touches[1].clientX;
    const dy = touches[0].clientY - touches[1].clientY;
    return Math.sqrt(dx * dx + dy * dy);
  }

})();
