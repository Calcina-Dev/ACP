// ==============================================================================
// SISTEMA DE EXPORTACIÓN DE RESÚMENES A IMAGEN (HTML2CANVAS) - AGRÍCOLA CERRO PRIETO
// ==============================================================================

(function() {
  "use strict";

  function mostrarToastExportacion(mensaje, tipo) {
    var toast = document.getElementById("toast_export_imagen");
    if (!toast) {
      toast = document.createElement("div");
      toast.id = "toast_export_imagen";
      toast.style.position = "fixed";
      toast.style.bottom = "24px";
      toast.style.right = "24px";
      toast.style.padding = "12px 22px";
      toast.style.borderRadius = "12px";
      toast.style.boxShadow = "0 8px 24px rgba(0,0,0,0.18)";
      toast.style.zIndex = "999999";
      toast.style.fontFamily = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif";
      toast.style.fontSize = "13.5px";
      toast.style.fontWeight = "600";
      toast.style.display = "flex";
      toast.style.alignItems = "center";
      toast.style.gap = "10px";
      toast.style.transition = "all 0.3s cubic-bezier(0.16, 1, 0.3, 1)";
      document.body.appendChild(toast);
    }
    
    if (tipo === "loading") {
      toast.style.background = "#1C1C1E";
      toast.style.color = "#FFFFFF";
      toast.innerHTML = '<span style="font-size:16px;">⏳</span> <span>' + mensaje + '</span>';
    } else if (tipo === "success") {
      toast.style.background = "#15803D";
      toast.style.color = "#FFFFFF";
      toast.innerHTML = '<span style="font-size:16px;">✅</span> <span>' + mensaje + '</span>';
      setTimeout(function() {
        if (toast) toast.style.opacity = "0";
        setTimeout(function() { if (toast && toast.parentNode) toast.parentNode.removeChild(toast); }, 300);
      }, 3500);
    } else if (tipo === "error") {
      toast.style.background = "#DC2626";
      toast.style.color = "#FFFFFF";
      toast.innerHTML = '<span style="font-size:16px;">⚠️</span> <span>' + mensaje + '</span>';
      setTimeout(function() {
        if (toast) toast.style.opacity = "0";
        setTimeout(function() { if (toast && toast.parentNode) toast.parentNode.removeChild(toast); }, 4000);
      }, 4000);
    }
    toast.style.opacity = "1";
    toast.style.transform = "translateY(0)";
  }

  function descargarCanvasPNG(canvas, filename) {
    if (!canvas) {
      console.error("[ACP Export] Canvas no válido para descarga");
      return;
    }
    try {
      if (canvas.toBlob) {
        canvas.toBlob(function(blob) {
          if (!blob) {
            fallbackDataUrl(canvas, filename);
            return;
          }
          var url = URL.createObjectURL(blob);
          var link = document.createElement("a");
          link.download = filename;
          link.href = url;
          document.body.appendChild(link);
          link.click();
          setTimeout(function() {
            if (link.parentNode) link.parentNode.removeChild(link);
            URL.revokeObjectURL(url);
          }, 2000);
        }, "image/png");
      } else {
        fallbackDataUrl(canvas, filename);
      }
    } catch(e) {
      console.warn("[ACP Export] toBlob falló, usando fallback dataURL:", e);
      fallbackDataUrl(canvas, filename);
    }
  }

  function fallbackDataUrl(canvas, filename) {
    try {
      var link = document.createElement("a");
      link.download = filename;
      link.href = canvas.toDataURL("image/png");
      document.body.appendChild(link);
      link.click();
      setTimeout(function() {
        if (link.parentNode) link.parentNode.removeChild(link);
      }, 1000);
    } catch(err) {
      console.error("[ACP Export] Error fatal al descargar imagen:", err);
    }
  }

  function capturarElemento(elemId) {
    return new Promise(function(resolve, reject) {
      var el = document.getElementById(elemId);
      if (!el) {
        console.error("[ACP Export] Elemento " + elemId + " no encontrado en el DOM");
        return reject(new Error("Elemento " + elemId + " no encontrado"));
      }

      // Asegurar que el contenedor tenga amplitud óptima para que todas las columnas
      // queden perfectamente extendidas con tipografía 15px sin apiñamientos
      var prevMinWidth = el.style.minWidth;
      var anchoTarget = Math.max(el.scrollWidth, el.offsetWidth, 1040);
      el.style.minWidth = anchoTarget + "px";

      var opt = {
        scale: 2,
        useCORS: true,
        allowTaint: true,
        backgroundColor: "#FFFFFF",
        logging: false
      };

      html2canvas(el, opt)
        .then(function(canvas) {
          el.style.minWidth = prevMinWidth;
          resolve(canvas);
        })
        .catch(function(err) {
          el.style.minWidth = prevMinWidth;
          console.error("[ACP Export] html2canvas error:", err);
          reject(err);
        });
    });
  }

  function stitchCanvases(canvases, titulo, subtitulo) {
    if (!canvases || canvases.length === 0) return null;
    
    var validCanvases = canvases.filter(function(c) { return c && c.width > 0 && c.height > 0; });
    if (validCanvases.length === 0) return null;
    
    var pad = 40;
    var headerH = 140;
    var maxWidth = 0;
    var totalHeight = headerH;
    
    validCanvases.forEach(function(c) {
      if (c.width > maxWidth) maxWidth = c.width;
      totalHeight += c.height + pad;
    });
    
    var master = document.createElement("canvas");
    master.width = maxWidth + (pad * 2);
    master.height = totalHeight + pad;
    var ctx = master.getContext("2d");
    
    // Fondo general ejecutivo
    ctx.fillStyle = "#F8FAFC";
    ctx.fillRect(0, 0, master.width, master.height);
    
    // Membrete Superior
    ctx.fillStyle = "#FFFFFF";
    ctx.fillRect(pad, pad, maxWidth, 90);
    
    ctx.strokeStyle = "#E2E8F0";
    ctx.lineWidth = 1;
    ctx.strokeRect(pad, pad, maxWidth, 90);
    
    // Franja azul corporativa decorativa
    ctx.fillStyle = "#4674B9";
    ctx.fillRect(pad, pad, 6, 90);
    
    // Título y Subtítulo
    ctx.fillStyle = "#0F172A";
    ctx.font = "bold 28px -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif";
    ctx.fillText(titulo || "AGRÍCOLA CERRO PRIETO · RESUMEN DE COSECHA", pad + 24, pad + 40);
    
    ctx.fillStyle = "#64748B";
    ctx.font = "600 16px -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif";
    ctx.fillText(subtitulo || "Reporte Semanal Consolidado", pad + 24, pad + 70);
    
    // Dibujar cada canvas centrado verticalmente
    var currentY = headerH + 20;
    validCanvases.forEach(function(c) {
      var x = pad + (maxWidth - c.width) / 2;
      ctx.drawImage(c, x, currentY);
      currentY += c.height + pad;
    });
    
    return master;
  }

  // Exportar solo la vista actualmente activa
  window.exportarImagenActual = function() {
    if (typeof html2canvas === "undefined") {
      alert("La biblioteca de captura se está inicializando. Por favor intenta de nuevo en unos segundos.");
      return;
    }
    
    var forma = $('input[name="switch_forma_resumen"]:checked').val() || "variedad";
    var elemId = "contenedor_resumen_variedad";
    var nombreForma = "Por_Variedad";
    
    if (forma === "modulo_variedad") {
      elemId = "contenedor_resumen_modulo_variedad";
      nombreForma = "Por_Modulo_y_Variedad";
    } else if (forma === "programa") {
      elemId = "contenedor_resumen_programa";
      nombreForma = "Distribucion_Comercial";
    }
    
    var sem = $("#filtro_semana").val() || "Sem";
    var camp = $("#filtro_campana").val() || "2026";
    var fechaStr = new Date().toISOString().slice(0, 10);
    var filename = "ACP_Resumen_" + nombreForma + "_Semana_" + sem + "_" + camp + "_" + fechaStr + ".png";
    
    mostrarToastExportacion("Generando imagen de alta resolución...", "loading");
    
    capturarElemento(elemId).then(function(canvas) {
      descargarCanvasPNG(canvas, filename);
      mostrarToastExportacion("Resumen exportado exitosamente", "success");
    }).catch(function(err) {
      console.error("[ACP Export] Error al exportar vista actual:", err);
      mostrarToastExportacion("Error al generar la imagen", "error");
    });
  };

  // Exportar todos los resúmenes secuencialmente
  window.exportarTodosLosResumenes = function(modo) {
    if (typeof html2canvas === "undefined") {
      alert("La biblioteca de captura se está inicializando. Por favor intenta de nuevo en unos segundos.");
      return;
    }
    
    // Cerrar modal de selección de forma limpia
    try {
      $(".modal").modal("hide");
      $(".modal-backdrop").remove();
      $("body").removeClass("modal-open").css("padding-right", "");
    } catch(mErr) {
      console.warn("Aviso al cerrar modal:", mErr);
    }
    
    mostrarToastExportacion("Procesando todos los resúmenes en alta resolución...", "loading");
    
    var initialRadio = $('input[name="switch_forma_resumen"]:checked').val() || "variedad";
    var sem = $("#filtro_semana").val() || "Sem";
    var camp = $("#filtro_campana").val() || "2026";
    var fechaStr = new Date().toISOString().slice(0, 10);
    
    function cambiarForma(val) {
      return new Promise(function(resolve) {
        $('input[name="switch_forma_resumen"][value="' + val + '"]').prop("checked", true).trigger("change");
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue("switch_forma_resumen", val);
        }
        setTimeout(resolve, 550);
      });
    }

    var c1 = null;
    var c2 = null;
    var c3 = null;

    cambiarForma("variedad")
      .then(function() {
        return capturarElemento("contenedor_resumen_variedad");
      })
      .then(function(canvas1) {
        c1 = canvas1;
        return cambiarForma("modulo_variedad");
      })
      .then(function() {
        return capturarElemento("contenedor_resumen_modulo_variedad");
      })
      .then(function(canvas2) {
        c2 = canvas2;
        return cambiarForma("programa");
      })
      .then(function() {
        return capturarElemento("contenedor_resumen_programa");
      })
      .then(function(canvas3) {
        c3 = canvas3;
        
        // Restaurar pestaña que el usuario estaba viendo
        cambiarForma(initialRadio);
        
        // Descargas individuales si corresponde
        if (modo === "individuales" || modo === "todo") {
          descargarCanvasPNG(c1, "ACP_Resumen_1_Por_Variedad_Semana_" + sem + "_" + camp + "_" + fechaStr + ".png");
          setTimeout(function() {
            descargarCanvasPNG(c2, "ACP_Resumen_2_Por_Modulo_Variedad_Semana_" + sem + "_" + camp + "_" + fechaStr + ".png");
          }, 350);
          setTimeout(function() {
            descargarCanvasPNG(c3, "ACP_Resumen_3_Distribucion_Comercial_Semana_" + sem + "_" + camp + "_" + fechaStr + ".png");
          }, 700);
        }
        
        // Descarga de reporte consolidado si corresponde
        if (modo === "consolidado" || modo === "todo") {
          var subT = "Campaña " + camp + " • Semana " + sem + " • Generado: " + new Date().toLocaleString();
          var masterCanvas = stitchCanvases([c1, c2, c3], "AGRÍCOLA CERRO PRIETO · REPORTE EJECUTIVO DE RESÚMENES", subT);
          if (masterCanvas) {
            var delayConsolidado = (modo === "todo" ? 1100 : 100);
            setTimeout(function() {
              descargarCanvasPNG(masterCanvas, "ACP_Reporte_Consolidado_Semana_" + sem + "_" + camp + "_" + fechaStr + ".png");
            }, delayConsolidado);
          }
        }
        
        mostrarToastExportacion("¡Resúmenes exportados con éxito!", "success");
      })
      .catch(function(err) {
        console.error("[ACP Export] Error en proceso de exportación completa:", err);
        cambiarForma(initialRadio);
        mostrarToastExportacion("Error al generar las imágenes", "error");
      });
  };

  // Event Listeners con delegación
  $(document).on("click", "#btn_exportar_imagen_actual", function(e) {
    e.preventDefault();
    window.exportarImagenActual();
  });

  $(document).on("click", "#btn_run_export_consolidado", function(e) {
    e.preventDefault();
    window.exportarTodosLosResumenes("consolidado");
  });

  $(document).on("click", "#btn_run_export_individuales", function(e) {
    e.preventDefault();
    window.exportarTodosLosResumenes("individuales");
  });

  $(document).on("click", "#btn_run_export_todo", function(e) {
    e.preventDefault();
    window.exportarTodosLosResumenes("todo");
  });

})();
