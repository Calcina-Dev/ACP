/**
 * ==============================================================================
 * GOOGLE APPS SCRIPT: WEBHOOK DE CONTROL DE RUTAS DE COSECHA DE ARÁNDANO
 * Proyecto: Módulo Interactivo de Mapas 2D - Fundo 2026
 * ==============================================================================
 * 
 * INSTRUCCIONES DE ACTUALIZACIÓN EN GOOGLE SHEETS:
 * 1. En tu Google Sheet, ve a: Extensiones > Apps Script.
 * 2. Reemplaza todo el código existente por este código completo.
 * 3. Clic en "Guardar" (ícono de disquete).
 * 4. Clic en el botón azul superior: "Implementar" (Deploy) > "Administrar implementaciones".
 * 5. Haz clic en el ícono de lápiz ✏️ para editar la implementación activa.
 * 6. En "Versión", selecciona: "Nueva versión" (New version).
 * 7. Clic en "Implementar" (Deploy) y "Listo" (Done).
 */

function doPost(e) {
  var lock = LockService.getScriptLock();
  lock.tryLock(25000); // Evitar bloqueos concurrentes
  
  try {
    var rawData = e.postData.contents;
    var data = JSON.parse(rawData);
    
    var ss = SpreadsheetApp.getActiveSpreadsheet();
    
    // Obtener o crear pestañas
    var sheetVigentes = ss.getSheetByName("Rutas_Vigentes");
    if (!sheetVigentes) {
      sheetVigentes = ss.insertSheet("Rutas_Vigentes");
      crearEncabezados(sheetVigentes);
    }
    
    var sheetAuditoria = ss.getSheetByName("Auditoria_Historial");
    if (!sheetAuditoria) {
      sheetAuditoria = ss.insertSheet("Auditoria_Historial");
      crearEncabezados(sheetAuditoria);
    }
    
    var campana   = data.campana;
    var semana    = data.semana;
    var modo      = data.modo; // "Proyección" o "Cosecha Campo"
    var pasada    = data.pasada;
    var evaluador = data.evaluador || "Evaluador Campo";
    var fechaHora = Utilities.formatDate(new Date(), "GMT-5", "yyyy-MM-dd HH:mm:ss");
    
    // MODO PARCIAL (multi-usuario): la app envía solo las válvulas propias del usuario (rutas_usuario) y el
    // listado de códigos que puede reemplazar (alcance_codigos). Las filas de OTROS usuarios no se tocan.
    // Si no vienen esos campos (app antigua), se mantiene el comportamiento anterior (reemplazo completo).
    var parcial = (data.parcial === true) && Array.isArray(data.alcance_codigos);
    var rutas   = parcial ? (data.rutas_usuario || []) : (data.rutas || []);
    
    // Sin cambios que registrar (alcance vacío y sin filas): no ensuciar el historial
    if (parcial && data.alcance_codigos.length === 0 && rutas.length === 0) {
      return ContentService.createTextOutput(JSON.stringify({
        status: "success",
        message: "Sin cambios en el alcance del usuario",
        version: "sin_cambios",
        filas_guardadas: 0,
        timestamp: fechaHora
      })).setMimeType(ContentService.MimeType.JSON);
    }
    
    // 1. Determinar número de versión real para Auditoría
    var versionNum = calcularSiguienteVersion(sheetAuditoria, campana, semana, modo, pasada);
    var versionTag = "v" + versionNum;
    
    // 2. Preparar matriz de filas para insertar
    var rowsToInsert = [];
    for (var i = 0; i < rutas.length; i++) {
      var r = rutas[i];
      var subTag = "";
      if (r.id && r.id.indexOf("_") !== -1) {
        var parts = r.id.split("_");
        if (parts.length > 2) {
          subTag = "_" + parts.slice(2).join("_");
        } else if (r.variedad) {
          subTag = "_" + String(r.variedad).trim().replace(/[^a-zA-Z0-9]/g, '');
        }
      } else if (r.variedad) {
        subTag = "_" + String(r.variedad).trim().replace(/[^a-zA-Z0-9]/g, '');
      }
      var idReg = campana + "_S" + semana + "_" + String(modo).replace(/\s+/g, '') + "_P" + pasada + "_" + r.codigo + subTag;
      
      rowsToInsert.push([
        idReg,
        fechaHora,
        versionTag,
        evaluador,
        campana,
        semana,
        modo,
        pasada,
        r.grupo || "",
        r.sector || "",
        r.modulo || "",
        r.turno || "",
        r.codigo || "",
        r.variedad || "",
        Number(r.area_fisica || 0),
        Number(r.pct_lun || 0), Number(r.ha_lun || 0), Number(r.kg_lun || 0),
        Number(r.pct_mar || 0), Number(r.ha_mar || 0), Number(r.kg_mar || 0),
        Number(r.pct_mie || 0), Number(r.ha_mie || 0), Number(r.kg_mie || 0),
        Number(r.pct_jue || 0), Number(r.ha_jue || 0), Number(r.kg_jue || 0),
        Number(r.pct_vie || 0), Number(r.ha_vie || 0), Number(r.kg_vie || 0),
        Number(r.pct_sab || 0), Number(r.ha_sab || 0), Number(r.kg_sab || 0),
        Number(r.pct_total || 0), Number(r.ha_total || 0), Number(r.kg_total || 0),
        r.cells_str || ""
      ]);
    }
    
    // 3. Eliminar la versión previa en Rutas_Vigentes (en memoria, operación Batch ultra rápida)
    //    - Parcial: solo las válvulas del alcance del usuario
    //    - Completo: todo el modo/pasada de la semana
    if (parcial) {
      eliminarCodigosPrevios(sheetVigentes, campana, semana, modo, pasada, data.alcance_codigos);
    } else {
      eliminarVersionPrevia(sheetVigentes, campana, semana, modo, pasada);
    }
    
    if (rowsToInsert.length > 0) {
      // Escribir en Auditoria_Historial (Append inmutable)
      var lastRowAud = sheetAuditoria.getLastRow();
      sheetAuditoria.getRange(lastRowAud + 1, 1, rowsToInsert.length, rowsToInsert[0].length).setValues(rowsToInsert);
      
      // Escribir las nuevas filas vigentes
      var lastRowVig = sheetVigentes.getLastRow();
      sheetVigentes.getRange(lastRowVig + 1, 1, rowsToInsert.length, rowsToInsert[0].length).setValues(rowsToInsert);
      
      return ContentService.createTextOutput(JSON.stringify({
        status: "success",
        message: "Rutas guardadas exitosamente",
        version: versionTag,
        filas_guardadas: rowsToInsert.length,
        timestamp: fechaHora
      })).setMimeType(ContentService.MimeType.JSON);
    } else {
      // Si la ruta se vació (borrado total), registrar la eliminación en Auditoria_Historial
      var idReg = campana + "_S" + semana + "_" + String(modo).replace(/\s+/g, '') + "_P" + pasada + "_ELIMINADA";
      var auditRow = [[
        idReg, fechaHora, versionTag, evaluador,
        campana, semana, modo, pasada,
        "ELIMINADA", "", "", "", "", "",
        0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 0,
        0, 0, 0,
        ""
      ]];
      var lastRowAud = sheetAuditoria.getLastRow();
      sheetAuditoria.getRange(lastRowAud + 1, 1, 1, auditRow[0].length).setValues(auditRow);
      
      return ContentService.createTextOutput(JSON.stringify({
        status: "success",
        message: "Ruta eliminada de Rutas_Vigentes",
        version: versionTag,
        filas_guardadas: 0,
        timestamp: fechaHora
      })).setMimeType(ContentService.MimeType.JSON);
    }
    
  } catch (error) {
    return ContentService.createTextOutput(JSON.stringify({
      status: "error",
      message: error.toString()
    })).setMimeType(ContentService.MimeType.JSON);
  } finally {
    lock.releaseLock();
  }
}

// Lectura directa desde URL (GET) con soporte para:
// 1. Usuarios: ?action=usuarios
// 2. Rutas vigentes: ?campana=2026&semana=XX
function doGet(e) {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var action = (e && e.parameter && (e.parameter.action || e.parameter.tipo)) ? 
               String(e.parameter.action || e.parameter.tipo).trim().toLowerCase() : "";

  // A. Consultar tabla de usuarios y permisos
  if (action === "usuarios") {
    var sheetUsuarios = ss.getSheetByName("Usuarios");
    if (!sheetUsuarios) {
      sheetUsuarios = ss.insertSheet("Usuarios");
      crearPestanaUsuarios(sheetUsuarios);
      insertarUsuariosDefault(sheetUsuarios);
    }
    
    var lastRowU = sheetUsuarios.getLastRow();
    if (lastRowU <= 1) {
      insertarUsuariosDefault(sheetUsuarios);
      lastRowU = sheetUsuarios.getLastRow();
    }
    
    var dataU = sheetUsuarios.getDataRange().getDisplayValues();
    var headersU = dataU[0];
    var resU = [];
    for (var u = 1; u < dataU.length; u++) {
      var rowU = dataU[u];
      if (!rowU[0] || String(rowU[0]).trim() === "") continue;
      var objU = {};
      for (var k = 0; k < headersU.length; k++) {
        var hName = String(headersU[k]).trim();
        objU[hName] = rowU[k];
      }
      resU.push(objU);
    }
    return ContentService.createTextOutput(JSON.stringify(resU)).setMimeType(ContentService.MimeType.JSON);
  }

  // B. Consultar rutas vigentes
  var sheetVigentes = ss.getSheetByName("Rutas_Vigentes");
  if (!sheetVigentes) {
    return ContentService.createTextOutput(JSON.stringify([])).setMimeType(ContentService.MimeType.JSON);
  }
  
  var lastRow = sheetVigentes.getLastRow();
  if (lastRow <= 1) {
    return ContentService.createTextOutput(JSON.stringify([])).setMimeType(ContentService.MimeType.JSON);
  }
  
  var data = sheetVigentes.getDataRange().getValues();
  var headers = data[0];
  var result = [];
  
  var filterSemana  = (e && e.parameter && e.parameter.semana)  ? String(e.parameter.semana).trim()  : null;
  var filterCampana = (e && e.parameter && e.parameter.campana) ? String(e.parameter.campana).trim() : null;
  
  for (var i = 1; i < data.length; i++) {
    var row = data[i];
    if (filterCampana && String(row[4]).trim() !== filterCampana) continue;
    if (filterSemana && String(row[5]).trim() !== filterSemana) continue;
    
    var obj = {};
    for (var j = 0; j < headers.length; j++) {
      obj[headers[j]] = row[j];
    }
    result.push(obj);
  }
  
  return ContentService.createTextOutput(JSON.stringify(result)).setMimeType(ContentService.MimeType.JSON);
}

function crearPestanaUsuarios(sheet) {
  var headers = ["Usuario", "Contrasena", "Nombre", "Rol", "Modulos"];
  sheet.getRange(1, 1, 1, headers.length).setValues([headers]);
  sheet.getRange(1, 1, 1, headers.length).setFontWeight("bold").setBackground("#E2E8F0");
  sheet.setFrozenRows(1);
}

function insertarUsuariosDefault(sheet) {
  var defaultUsers = [
    ["cmendoza",     "76174232", "Carlos Mendoza Calcina",         "Administrador", "1-14"],
    ["ksantisteban", "75182402", "Katty Ideth Santisteban Huanila", "Sectorista",    "1, 2, 3"],
    ["ldelgado",     "75210386", "Lucy Mirella Delgado Inoñan",     "Sectorista",    "10, 13, 14"]
  ];
  var rng = sheet.getRange(2, 1, defaultUsers.length, defaultUsers[0].length);
  rng.setNumberFormat("@");
  rng.setValues(defaultUsers);
}

// Funciones auxiliares optimizadas
function crearEncabezados(sheet) {
  var headers = [
    "ID_Registro", "Fecha_Hora", "Version", "Evaluador",
    "Campaña", "Semana", "Modo", "Pasada",
    "Grupo_Modulo", "Sector", "Módulo", "Turno", "Codigo_Valvula", "Variedad", "Area_Fisica_Ha",
    "Pct_Lunes", "Ha_Lunes", "Kg_Lunes",
    "Pct_Martes", "Ha_Martes", "Kg_Martes",
    "Pct_Miercoles", "Ha_Miercoles", "Kg_Miercoles",
    "Pct_Jueves", "Ha_Jueves", "Kg_Jueves",
    "Pct_Viernes", "Ha_Viernes", "Kg_Viernes",
    "Pct_Sabado", "Ha_Sabado", "Kg_Sabado",
    "Pct_Total_Sem", "Ha_Total_Sem", "Kg_Total_Sem",
    "Cells_Str"
  ];
  sheet.getRange(1, 1, 1, headers.length).setValues([headers]);
  sheet.getRange(1, 1, 1, headers.length).setFontWeight("bold").setBackground("#F1F5F9");
  sheet.setFrozenRows(1);
}

function calcularSiguienteVersion(sheet, campana, semana, modo, pasada) {
  var lastRow = sheet.getLastRow();
  if (lastRow <= 1) return 1;
  
  var data = sheet.getRange(2, 1, lastRow - 1, 8).getValues();
  var maxVersion = 0;
  for (var i = 0; i < data.length; i++) {
    var row = data[i];
    var match = (String(row[4]).trim() === String(campana).trim()) &&
                (String(row[5]).trim() === String(semana).trim()) &&
                (String(row[6]).trim() === String(modo).trim()) &&
                (String(row[7]).trim() === String(pasada).trim());
    if (match) {
      var vStr = String(row[2] || '');
      var vNum = parseInt(vStr.replace(/[^0-9]/g, ''), 10);
      if (!isNaN(vNum) && vNum > maxVersion) {
        maxVersion = vNum;
      }
    }
  }
  return maxVersion + 1;
}

function eliminarVersionPrevia(sheet, campana, semana, modo, pasada) {
  var lastRow = sheet.getLastRow();
  if (lastRow <= 1) return;
  
  var lastCol = sheet.getLastColumn();
  if (lastCol < 8) return;
  
  var data = sheet.getRange(1, 1, lastRow, lastCol).getValues();
  var headers = data[0];
  var rowsToKeep = [headers];
  
  for (var i = 1; i < data.length; i++) {
    var row = data[i];
    var match = (String(row[4]).trim() === String(campana).trim()) &&
                (String(row[5]).trim() === String(semana).trim()) &&
                (String(row[6]).trim() === String(modo).trim()) &&
                (String(row[7]).trim() === String(pasada).trim());
    if (!match) {
      rowsToKeep.push(row);
    }
  }
  
  if (rowsToKeep.length < data.length) {
    sheet.clearContents();
    sheet.getRange(1, 1, rowsToKeep.length, headers.length).setValues(rowsToKeep);
    crearEncabezados(sheet);
  }
}

// Elimina de Rutas_Vigentes SOLO las válvulas (por Codigo_Valvula) incluidas en el alcance del usuario,
// para la campaña/semana/modo/pasada indicados. Las filas de otros usuarios permanecen intactas.
function eliminarCodigosPrevios(sheet, campana, semana, modo, pasada, codigos) {
  var lastRow = sheet.getLastRow();
  if (lastRow <= 1) return;
  
  var lastCol = sheet.getLastColumn();
  if (lastCol < 13) return;
  
  var setCodigos = {};
  for (var c = 0; c < codigos.length; c++) {
    setCodigos[String(codigos[c]).trim()] = true;
  }
  
  var data = sheet.getRange(1, 1, lastRow, lastCol).getValues();
  var headers = data[0];
  var rowsToKeep = [headers];
  
  for (var i = 1; i < data.length; i++) {
    var row = data[i];
    var mismoGrupo = (String(row[4]).trim() === String(campana).trim()) &&
                     (String(row[5]).trim() === String(semana).trim()) &&
                     (String(row[6]).trim() === String(modo).trim()) &&
                     (String(row[7]).trim() === String(pasada).trim());
    var enAlcance = setCodigos[String(row[12]).trim()] === true; // col 13 = Codigo_Valvula
    if (!(mismoGrupo && enAlcance)) {
      rowsToKeep.push(row);
    }
  }
  
  if (rowsToKeep.length < data.length) {
    sheet.clearContents();
    sheet.getRange(1, 1, rowsToKeep.length, headers.length).setValues(rowsToKeep);
    crearEncabezados(sheet);
  }
}

