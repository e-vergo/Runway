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
