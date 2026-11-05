// ==============================================================================
// Analisis de Productividad - Client Interactions
// Emil Kowalski Craft Micro-interactions & Sonner Toasts
// ==============================================================================

// Sonner Toast Notification Manager
window.showSonnerToast = function(message, icon = '', showUndo = false) {
  let container = document.getElementById('sonner-toast-container');
  if (!container) {
    container = document.createElement('div');
    container.id = 'sonner-toast-container';
    document.body.appendChild(container);
  }

  const toast = document.createElement('div');
  toast.className = 'sonner-toast';

  let undoHtml = showUndo ? `<button class="sonner-undo-btn" onclick="triggerUndo(this)">Deshacer</button>` : '';
  let iconHtml = (icon && icon.trim() !== '') ? `<span class="sonner-icon">${icon}</span>` : '';

  toast.innerHTML = `
    <div class="sonner-content">
      ${iconHtml}
      <span>${message}</span>
    </div>
    ${undoHtml}
  `;

  container.appendChild(toast);

  // Auto dismiss after 3.8s
  setTimeout(() => {
    toast.classList.add('leaving');
    setTimeout(() => {
      if (toast.parentNode) toast.parentNode.removeChild(toast);
    }, 200);
  }, 3800);
};

window.triggerUndo = function(btn) {
  const toast = btn.closest('.sonner-toast');
  if (toast) {
    toast.classList.add('leaving');
    setTimeout(() => toast.remove(), 200);
  }
  if (window.Shiny) {
    Shiny.setInputValue('btn_undo_last_edit', Date.now(), { priority: 'event' });
  }
};

// Handle Cell Click from Gantt
window.handleCellClick = function(unidad, colKey, stage) {
  if (window.Shiny) {
    Shiny.setInputValue('gantt_cell_clicked', {
      unidad: unidad,
      colKey: colKey,
      stage: stage,
      timestamp: Date.now()
    }, { priority: 'event' });
  }
};

// Client-side Legend stage highlight toggle
window.highlightStage = function(stageName) {
  if (window.Shiny) {
    Shiny.setInputValue('legend_stage_selected', stageName, { priority: 'event' });
  }
  
  // Instant visual dimming via CSS
  const cells = document.querySelectorAll('.gantt-cell.has-stage');
  if (!stageName || stageName === 'Todos') {
    cells.forEach(el => {
      el.style.opacity = '1';
      el.style.filter = 'none';
      el.style.transform = 'none';
    });
    return;
  }

  cells.forEach(el => {
    const s = el.getAttribute('data-stage');
    if (s === stageName) {
      el.style.opacity = '1';
      el.style.filter = 'none';
      el.style.transform = 'scaleY(1.08)';
      el.style.zIndex = '10';
    } else {
      el.style.opacity = '0.15';
      el.style.filter = 'grayscale(50%)';
      el.style.transform = 'none';
      el.style.zIndex = '1';
    }
  });
};

// Tab Pill Visual Active Toggle
$(document).on('click', '.segmented-tab-container .tab-pill', function() {
  $('.segmented-tab-container .tab-pill').removeClass('active');
  $(this).addClass('active');
});

// Year Type Pill Toggle: Handled reactively via active_year_types_changed

// Popover Dropdown for Options & Scenarios
$(document).on('click', '#btn_toggle_options', function(e) {
  e.stopPropagation();
  $('#popover_notifications_menu').removeClass('show');
  $('#popover_climate_menu').removeClass('show');
  $('#popover_options_menu').toggleClass('show');
});

$(document).on('click', '#popover_options_menu', function(e) {
  e.stopPropagation();
});

// Popover Dropdown for Climate Variables
$(document).on('click', '#btn_toggle_climate', function(e) {
  e.stopPropagation();
  $('#popover_options_menu').removeClass('show');
  $('#popover_notifications_menu').removeClass('show');
  $('#popover_climate_menu').toggleClass('show');
});

$(document).on('click', '#popover_climate_menu', function(e) {
  e.stopPropagation();
});

// Popover Dropdown for Subtle Projection Notifications
$(document).on('click', '#btn_toggle_notifications', function(e) {
  e.stopPropagation();
  $('#popover_options_menu').removeClass('show');
  $('#popover_climate_menu').removeClass('show');
  $('#popover_notifications_menu').toggleClass('show');
});

$(document).on('click', '#popover_notifications_menu', function(e) {
  e.stopPropagation();
});

$(document).on('click', function() {
  $('#popover_options_menu').removeClass('show');
  $('#popover_notifications_menu').removeClass('show');
  $('#popover_climate_menu').removeClass('show');
});

// Helper for iOS Switch toggles
window.handleSwitchToggle = function(switchId) {
  const el = document.getElementById(switchId);
  if (el && window.Shiny) {
    Shiny.setInputValue(switchId, el.checked, { priority: 'event' });
  }
};

// Dedicated handler for Dynamic Historical Campaign Switches (prevents reactive re-rendering loops)
window.handleHistoricalSwitchToggle = function(switchId, campLabel) {
  const el = document.getElementById(switchId);
  if (el && window.Shiny) {
    Shiny.setInputValue('historical_campaign_toggled', {
      campaign: campLabel,
      active: el.checked,
      timestamp: Date.now()
    }, { priority: 'event' });
  }
};

// Listen for Shiny Custom Messages
$(document).on('shiny:connected', function() {
  Shiny.addCustomMessageHandler('show_toast', function(data) {
    showSonnerToast(data.message, data.icon || '', data.showUndo || false);
  });
  
  Shiny.addCustomMessageHandler('active_tab_changed', function(tabName) {
    $('.segmented-tab-container .tab-pill').removeClass('active');
    if (tabName === 'gantt') $('#tab_btn_gantt').addClass('active');
    if (tabName === 'bloques') $('#tab_btn_bloques').addClass('active');
    if (tabName === 'matriz') $('#tab_btn_matriz').addClass('active');
  });

  Shiny.addCustomMessageHandler('active_year_types_changed', function(types) {
    if (types && types.indexOf('Año neutral') !== -1) {
      $('#btn_year_neutral').addClass('active');
    } else {
      $('#btn_year_neutral').removeClass('active');
    }
    if (types && types.indexOf('Año caliente') !== -1) {
      $('#btn_year_caliente').addClass('active');
    } else {
      $('#btn_year_caliente').removeClass('active');
    }
  });

  Shiny.addCustomMessageHandler('active_metric_changed', function(metric) {
    $('.ios-seg-control .ios-seg-btn').removeClass('active');
    if (metric === 'semanal') $('#btn_metric_semanal').addClass('active');
    if (metric === 'acumulado') $('#btn_metric_acumulado').addClass('active');
  });

  Shiny.addCustomMessageHandler('active_unit_changed', function(unit) {
    $('#btn_unit_kg_planta, #btn_unit_kg_total').removeClass('active');
    if (unit === 'kg_planta') $('#btn_unit_kg_planta').addClass('active');
    if (unit === 'kg_total') $('#btn_unit_kg_total').addClass('active');
  });

  Shiny.addCustomMessageHandler('active_view_changed', function(view) {
    $('#btn_metric_semanal, #btn_metric_acumulado').removeClass('active');
    if (view === 'semanal') $('#btn_metric_semanal').addClass('active');
    if (view === 'acumulado') $('#btn_metric_acumulado').addClass('active');
  });
});
