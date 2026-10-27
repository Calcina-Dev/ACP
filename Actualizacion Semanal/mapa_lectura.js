// ==============================================================================
// MOTOR CANVAS 2D DE MAPA DE COSECHA OFICIAL (SOLO LECTURA)
// ==============================================================================

let HARVEST_DATA = [];

if (typeof VALVES_MAPA !== 'undefined' && Array.isArray(VALVES_MAPA)) {
  VALVES_MAPA.forEach(v => {
    if (v.poligono_rel && typeof v.poligono_rel === 'string' && v.poligono_rel.startsWith('[')) {
      try { v.pts = JSON.parse(v.poligono_rel); } catch(e) { v.pts = null; }
    } else {
      v.pts = null;
    }
  });
}

let harvestMap = new Map();
function buildHarvestMap(valvulas) {
  harvestMap.clear();
  if (!valvulas) return;
  
  function saveEntry(key, item) {
    if (!key) return;
    const cleanKey = String(key).trim().toUpperCase();
    harvestMap.set(cleanKey, item);
  }
  
  if (Array.isArray(valvulas)) {
    for (let i = 0; i < valvulas.length; i++) {
      const d = valvulas[i];
      if (d) {
        if (d.id) saveEntry(d.id, d);
        if (d.codigo) saveEntry(d.codigo, d);
      }
    }
  } else if (typeof valvulas === "object" && Array.isArray(valvulas.id)) {
    const ids = valvulas.id;
    const n = ids.length;
    const keys = Object.keys(valvulas);
    for (let i = 0; i < n; i++) {
      const row = {};
      for (let k = 0; k < keys.length; k++) {
        const col = keys[k];
        row[col] = valvulas[col][i];
      }
      if (ids[i]) saveEntry(ids[i], row);
      if (row.codigo) saveEntry(row.codigo, row);
    }
  }
}

const DAY_DAYS = [
  { key: "Pct_Lunes",     kgKey: "Kg_Lunes",     cellKey: "lunes",     name: "Lunes",     color: "#EF4444" },
  { key: "Pct_Martes",    kgKey: "Kg_Martes",    cellKey: "martes",    name: "Martes",    color: "#0EA5E9" },
  { key: "Pct_Miercoles", kgKey: "Kg_Miercoles", cellKey: "miercoles", name: "Miércoles", color: "#F97316" },
  { key: "Pct_Jueves",    kgKey: "Kg_Jueves",    cellKey: "jueves",    name: "Jueves",    color: "#EAB308" },
  { key: "Pct_Viernes",   kgKey: "Kg_Viernes",   cellKey: "viernes",   name: "Viernes",   color: "#22C55E" },
  { key: "Pct_Sabado",    kgKey: "Kg_Sabado",    cellKey: "sabado",    name: "Sábado",    color: "#A855F7" }
];
const COLOR_BY_CELL_DAY = {};
DAY_DAYS.forEach(d => { COLOR_BY_CELL_DAY[d.cellKey] = d.color; });

function buildReadonlyRanges(hData) {
  if (!hData) return [];
  const counts = {};
  let totalCount = 0;
  for (let d = 0; d < DAY_DAYS.length; d++) {
    const def = DAY_DAYS[d];
    const pct = Math.max(0, Math.min(100, Number(hData[def.key]) || 0));
    counts[def.cellKey] = pct;
    totalCount += pct;
  }
  if (totalCount <= 0 && Number(hData.Pct_Total) <= 0 && Number(hData.Kg_Total) <= 0) return [];

  if (hData.cells_str && typeof hData.cells_str === "string" && hData.cells_str.replace(/,/g, "").trim().length > 0) {
    const parts = hData.cells_str.split(",");
    const cells = [];
    const cellDays = {};
    for (let i = 0; i < 10; i++) {
      const ck = (parts[i] || "").trim().toLowerCase();
      cells.push(ck);
      if (ck && COLOR_BY_CELL_DAY[ck]) {
        cellDays[ck] = (cellDays[ck] || 0) + 10;
      }
    }
    let allMatch = true;
    for (let d = 0; d < DAY_DAYS.length; d++) {
      const ck = DAY_DAYS[d].cellKey;
      if (Math.abs((cellDays[ck] || 0) - (counts[ck] || 0)) >= 0.5) {
        allMatch = false;
        break;
      }
    }
    if (allMatch) {
      const res = [];
      for (let i = 0; i < 10; i++) {
        const ck = cells[i];
        if (!ck || !COLOR_BY_CELL_DAY[ck]) continue;
        const cStart = i * 10, cEnd = (i + 1) * 10;
        if (res.length > 0 && res[res.length - 1].day === ck && Math.abs(res[res.length - 1].end - cStart) < 0.05) {
          res[res.length - 1].end = cEnd;
        } else {
          res.push({ day: ck, color: COLOR_BY_CELL_DAY[ck], start: cStart, end: cEnd });
        }
      }
      return res;
    }

    // Porcentajes decimales manuales con posición definida en cells_str
    const groups = [];
    for (let i = 0; i < 10; i++) {
      const ck = cells[i];
      if (!ck || !COLOR_BY_CELL_DAY[ck]) continue;
      if (groups.length > 0 && groups[groups.length - 1].day === ck && groups[groups.length - 1].lastIdx === i - 1) {
        groups[groups.length - 1].lastIdx = i;
        groups[groups.length - 1].cellCount++;
      } else {
        groups.push({ day: ck, firstIdx: i, lastIdx: i, cellCount: 1 });
      }
    }
    const dayCellTotals = {};
    for (let g of groups) dayCellTotals[g.day] = (dayCellTotals[g.day] || 0) + g.cellCount;

    const placed = [];
    for (let g of groups) {
      const dayPct = counts[g.day] || 0;
      if (dayPct <= 0) continue;
      const share = dayCellTotals[g.day] > 0 ? (g.cellCount / dayCellTotals[g.day]) : 1;
      const gPct = Math.round(dayPct * share * 100) / 100;
      if (gPct <= 0) continue;
      const gStart = g.firstIdx * 10;
      const gEnd = (g.lastIdx + 1) * 10;
      let start = gStart;
      if (g.lastIdx === 9 && g.firstIdx > 0) {
        start = Math.max(0, 100 - gPct);
      } else if (g.firstIdx === 0 && g.lastIdx < 9) {
        start = 0;
      } else if (g.firstIdx > 0 && g.lastIdx < 9) {
        const gMid = (gStart + gEnd) / 2;
        start = Math.max(0, Math.min(100 - gPct, Math.round((gMid - gPct / 2) * 100) / 100));
      }
      placed.push({ day: g.day, color: COLOR_BY_CELL_DAY[g.day], start: start, end: Math.min(100, start + gPct) });
    }
    if (placed.length > 0) return placed;
  }

  const fallback = [];
  let cur = 0;
  for (let d = 0; d < DAY_DAYS.length; d++) {
    const def = DAY_DAYS[d];
    const pct = counts[def.cellKey] || 0;
    if (pct <= 0) continue;
    fallback.push({ day: def.cellKey, color: def.color, start: cur, end: Math.min(100, cur + pct) });
    cur += pct;
  }
  return fallback;
}

let canvas, ctx, wrapper, tooltip;
let scale = 1, panX = 0, panY = 0;
let isPanning = false, startX = 0, startY = 0;
let hoveredValve = null;
let filtroModulo = 'Todos';
let filteredIds = null;

let minX = 999999, minY = 999999, maxX = -999999, maxY = -999999;
if (typeof VALVES_MAPA !== 'undefined' && Array.isArray(VALVES_MAPA)) {
  VALVES_MAPA.forEach(v => {
    if (v.x < minX) minX = v.x;
    if (v.y < minY) minY = v.y;
    if ((v.x + v.w) > maxX) maxX = v.x + v.w;
    if ((v.y + v.h) > maxY) maxY = v.y + v.h;
  });
}

function isHorizontalValve(v) {
  const m = Number(v.Módulo);
  return m === 1 || m === 2 || m === 3;
}

function isPointInValve(wx, wy, v) {
  if (wx < v.x || wx > (v.x + v.w) || wy < v.y || wy > (v.y + v.h)) return false;
  if (!v.pts || v.pts.length < 3) return true;
  let inside = false;
  const n = v.pts.length;
  for (let i = 0, j = n - 1; i < n; j = i++) {
    const xi = v.x + v.pts[i][0] * v.w, yi = v.y + v.pts[i][1] * v.h;
    const xj = v.x + v.pts[j][0] * v.w, yj = v.y + v.pts[j][1] * v.h;
    const intersect = ((yi > wy) !== (yj > wy)) && (wx < (xj - xi) * (wy - yi) / (yj - yi) + xi);
    if (intersect) inside = !inside;
  }
  return inside;
}

function findValveAt(wx, wy) {
  if (typeof VALVES_MAPA === 'undefined' || !Array.isArray(VALVES_MAPA)) return null;
  for (let i = 0; i < VALVES_MAPA.length; i++) {
    const v = VALVES_MAPA[i];
    if (isPointInValve(wx, wy, v)) return v;
  }
  return null;
}

function resetView() {
  if (!wrapper || !canvas || typeof VALVES_MAPA === "undefined") return;
  const rect = wrapper.getBoundingClientRect();
  const effectiveW = (rect.width > 0 ? rect.width : (wrapper.offsetWidth > 0 ? wrapper.offsetWidth : 1200));
  const effectiveH = (rect.height > 0 ? rect.height : (wrapper.offsetHeight > 0 ? wrapper.offsetHeight : 620));
  const padding = 35;
  
  let targetValves = VALVES_MAPA;
  if (filteredIds && filteredIds.size > 0) {
    targetValves = VALVES_MAPA.filter(v => {
      const vid = String(v.id || "").trim().toUpperCase();
      const vcode = String(v.codigo || "").trim().toUpperCase();
      return filteredIds.has(vid) || (vcode && filteredIds.has(vcode));
    });
  }
  
  if (targetValves.length === 0) {
    targetValves = VALVES_MAPA;
  }
  
  let bMinX = Infinity, bMaxX = -Infinity, bMinY = Infinity, bMaxY = -Infinity;
  for (let i = 0; i < targetValves.length; i++) {
    const v = targetValves[i];
    if (v.x < bMinX) bMinX = v.x;
    if ((v.x + v.w) > bMaxX) bMaxX = v.x + v.w;
    if (v.y < bMinY) bMinY = v.y;
    if ((v.y + v.h) > bMaxY) bMaxY = v.y + v.h;
  }
  
  if (bMinX === Infinity) return;
  
  const boxW = Math.max(10, bMaxX - bMinX);
  const boxH = Math.max(10, bMaxY - bMinY);
  const cX = (bMinX + bMaxX) / 2;
  const cY = (bMinY + bMaxY) / 2;
  
  const availW = Math.max(50, effectiveW - padding * 2);
  const availH = Math.max(50, effectiveH - padding * 2);
  
  const scaleX = availW / boxW;
  const scaleY = availH / boxH;
  let fitScale = Math.min(scaleX, scaleY);
  
  const isFullFarm = (targetValves.length >= VALVES_MAPA.length - 10);
  if (isFullFarm) {
    scale = Math.min(Math.max(fitScale, 0.05), 1.2);
  } else {
    scale = Math.min(Math.max(fitScale, 0.08), 2.2);
  }
  
  panX = (effectiveW / 2) - cX * scale;
  panY = (effectiveH / 2) - cY * scale;
  render();
}

function resizeCanvas() {
  if (!wrapper || !canvas) return;
  const rect = wrapper.getBoundingClientRect();
  const dpr = window.devicePixelRatio || 1;
  const effectiveW = (rect.width > 0 ? rect.width : (wrapper.offsetWidth > 0 ? wrapper.offsetWidth : 1200));
  const effectiveH = (rect.height > 0 ? rect.height : (wrapper.offsetHeight > 0 ? wrapper.offsetHeight : 620));
  canvas.width = effectiveW * dpr;
  canvas.height = effectiveH * dpr;
  render();
}

function render() {
  if (!ctx || !canvas || typeof VALVES_MAPA === 'undefined') return;
  const dpr = window.devicePixelRatio || 1;
  const w = canvas.width / dpr;
  const h = canvas.height / dpr;
  
  ctx.save();
  ctx.scale(dpr, dpr);
  ctx.fillStyle = '#F8FAFC';
  ctx.fillRect(0, 0, w, h);
  
  ctx.save();
  ctx.translate(panX, panY);
  ctx.scale(scale, scale);
  
  const viewLeft   = -panX / scale;
  const viewTop    = -panY / scale;
  const viewRight  = (w - panX) / scale;
  const viewBottom = (h - panY) / scale;
  
  for (let i = 0; i < VALVES_MAPA.length; i++) {
    const v = VALVES_MAPA[i];
    if ((v.x + v.w) < viewLeft || v.x > viewRight || (v.y + v.h) < viewTop || v.y > viewBottom) {
      continue;
    }
    
    const vid = String(v.id || "").trim().toUpperCase();
    const vcode = String(v.codigo || "").trim().toUpperCase();
    const isFiltered = (filteredIds === null || filteredIds.has(vid) || (vcode && filteredIds.has(vcode)));
    ctx.globalAlpha = isFiltered ? 1.0 : 0.16;
    
    const hData = harvestMap.get(vid) || (vcode ? harvestMap.get(vcode) : null);
    const horiz = isHorizontalValve(v);
    const hasPoly = (v.pts && v.pts.length >= 3);
    
    if (hasPoly) {
      ctx.save();
      ctx.beginPath();
      ctx.moveTo(v.x + v.pts[0][0] * v.w, v.y + v.pts[0][1] * v.h);
      for (let p = 1; p < v.pts.length; p++) {
        ctx.lineTo(v.x + v.pts[p][0] * v.w, v.y + v.pts[p][1] * v.h);
      }
      ctx.closePath();
      ctx.clip();
    }
    
    // Fondo blanco de la válvula
    ctx.fillStyle = '#FFFFFF';
    ctx.fillRect(v.x, v.y, v.w, v.h);
    
    // Relleno por día cosechado (respetando posición física de cells_str)
    if (hData && (Number(hData.Pct_Total) > 0 || Number(hData.Kg_Total) > 0)) {
      const segs = buildReadonlyRanges(hData);
      for (let s = 0; s < segs.length; s++) {
        const seg = segs[s];
        const pct = seg.end - seg.start;
        if (pct <= 0) continue;
        ctx.fillStyle = seg.color;
        if (horiz) {
          const y = v.y + (seg.start / 100) * v.h;
          const hSeg = (pct / 100) * v.h;
          ctx.fillRect(v.x, y, v.w, hSeg);
        } else {
          const x = v.x + (seg.start / 100) * v.w;
          const wSeg = (pct / 100) * v.w;
          ctx.fillRect(x, v.y, wSeg, v.h);
        }
      }
    }
    
    if (hasPoly) {
      ctx.restore();
      ctx.strokeStyle = (hoveredValve && hoveredValve.id === v.id) ? '#007AFF' : (isFiltered ? '#000000' : 'rgba(0,0,0,0.25)');
      ctx.lineWidth = (hoveredValve && hoveredValve.id === v.id) ? Math.max(2.4 / scale, 1.8) : Math.max(1.2 / scale, 0.8);
      ctx.beginPath();
      ctx.moveTo(v.x + v.pts[0][0] * v.w, v.y + v.pts[0][1] * v.h);
      for (let p = 1; p < v.pts.length; p++) {
        ctx.lineTo(v.x + v.pts[p][0] * v.w, v.y + v.pts[p][1] * v.h);
      }
      ctx.closePath();
      ctx.stroke();
    } else {
      ctx.strokeStyle = (hoveredValve && hoveredValve.id === v.id) ? '#007AFF' : (isFiltered ? '#000000' : 'rgba(0,0,0,0.25)');
      ctx.lineWidth = (hoveredValve && hoveredValve.id === v.id) ? Math.max(2.4 / scale, 1.8) : Math.max(1.2 / scale, 0.8);
      ctx.strokeRect(v.x, v.y, v.w, v.h);
    }
    
    // Muesca superior
    const isTriangle = hasPoly && v.pts.length === 3;
    const isSlopedTop = hasPoly && !isTriangle && (v.pts.some(p => p[0] < 0.20 && p[1] > 0.40) || (v.pts[0][0] < 0.05 && v.pts[0][1] > 0.40));
    if (!isSlopedTop && !isTriangle) {
      const notchW = v.w * 0.22;
      const notchH = Math.max(v.h * 0.08, 4);
      ctx.fillStyle = isFiltered ? '#000000' : 'rgba(0,0,0,0.25)';
      ctx.fillRect(v.x + (v.w - notchW) / 2, v.y - notchH, notchW, notchH);
    }
    
    // Textos dentro de la válvula
    if (scale > 0.12 && v.w >= 18) {
      ctx.save();
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.strokeStyle = 'rgba(255, 255, 255, 0.90)';
      ctx.fillStyle = '#000000';
      ctx.lineJoin = 'round';
      
      const cx = isTriangle ? (v.x + v.w * 0.68) : (v.x + v.w * (isSlopedTop ? 0.60 : 0.50));
      const maxTextW = v.w * (isTriangle ? 0.58 : 0.88);
      const ry1 = isTriangle ? 0.58 : (isSlopedTop ? (v.pts[0][1] < 0.80 ? 0.42 : 0.36) : 0.27);
      const ry2 = isTriangle ? 0.74 : (isSlopedTop ? (v.pts[0][1] < 0.80 ? 0.64 : 0.60) : 0.55);
      const ry3 = isTriangle ? 0.88 : (isSlopedTop ? (v.pts[0][1] < 0.80 ? 0.85 : 0.83) : 0.81);
      
      if (scale > 0.20) {
        let fSizeCode = Math.min(19, Math.max(8, Math.floor(v.w * 0.13)));
        if (v.w < 90) fSizeCode = Math.min(10.5, fSizeCode);
        ctx.font = 'bold ' + fSizeCode + 'px Inter, -apple-system, sans-serif';
        let codeW = ctx.measureText(v.codigo).width;
        if (codeW > maxTextW) {
          fSizeCode = Math.max(7, Math.floor(fSizeCode * (maxTextW / codeW)));
          ctx.font = 'bold ' + fSizeCode + 'px Inter, -apple-system, sans-serif';
        }
        ctx.lineWidth = Math.min(Math.max(2.4 / scale, 1.2), Math.max(1.1, fSizeCode * 0.22));
        const y1 = v.y + v.h * ry1;
        ctx.strokeText(v.codigo, cx, y1);
        ctx.fillText(v.codigo, cx, y1);
        
        let fSizeVar = Math.min(13, Math.max(6.5, Math.round(fSizeCode * 0.68)));
        ctx.font = '600 ' + fSizeVar + 'px Inter, -apple-system, sans-serif';
        const varStr = v.Variedad || '';
        let varW = ctx.measureText(varStr).width;
        if (varW > maxTextW) {
          fSizeVar = Math.max(6, Math.floor(fSizeVar * (maxTextW / varW)));
          ctx.font = '600 ' + fSizeVar + 'px Inter, -apple-system, sans-serif';
        }
        const y2 = v.y + v.h * ry2;
        ctx.strokeText(varStr, cx, y2);
        ctx.fillText(varStr, cx, y2);
        
        const kgVal = (hData && hData.Kg_Total) ? Math.round(hData.Kg_Total) : 0;
        const textHa = (v.Área !== undefined && v.Área !== null ? v.Área : '') + ' ha' +
                       (kgVal > 0 ? ' • ' + kgVal.toLocaleString('en-US') + ' kg' : '');
        let fSizeHa = Math.min(12, Math.max(6.2, Math.round(fSizeCode * 0.62)));
        ctx.font = '700 ' + fSizeHa + 'px Inter, -apple-system, sans-serif';
        let haW = ctx.measureText(textHa).width;
        if (haW > maxTextW) {
          fSizeHa = Math.max(5.8, Math.floor(fSizeHa * (maxTextW / haW)));
          ctx.font = '700 ' + fSizeHa + 'px Inter, -apple-system, sans-serif';
        }
        const y3 = v.y + v.h * ry3;
        ctx.strokeText(textHa, cx, y3);
        ctx.fillText(textHa, cx, y3);
      } else {
        let fSizeZoom = Math.min(21, Math.max(8, Math.floor(v.w * 0.15)));
        ctx.font = 'bold ' + fSizeZoom + 'px Inter, -apple-system, sans-serif';
        let zW = ctx.measureText(v.codigo).width;
        if (zW > maxTextW) {
          fSizeZoom = Math.max(7, Math.floor(fSizeZoom * (maxTextW / zW)));
          ctx.font = 'bold ' + fSizeZoom + 'px Inter, -apple-system, sans-serif';
        }
        ctx.lineWidth = Math.min(Math.max(2.8 / scale, 1.4), Math.max(1.3, fSizeZoom * 0.25));
        const midY = isTriangle ? 0.72 : (isSlopedTop ? 0.62 : 0.50);
        ctx.strokeText(v.codigo, cx, v.y + v.h * midY);
        ctx.fillText(v.codigo, cx, v.y + v.h * midY);
      }
      ctx.restore();
    } else if (scale > 0.35 && v.w >= 3 && v.w < 18) {
      ctx.save();
      ctx.translate(v.x + v.w / 2, v.y + v.h / 2);
      ctx.rotate(-Math.PI / 2);
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      const vStr = ((v.Variedad ? v.Variedad + ' ' : '') + (v.Área !== undefined ? v.Área + ' ha' : '')).trim();
      let fSizeVert = Math.min(v.w * 0.76, 11);
      ctx.font = 'bold ' + fSizeVert + 'px Inter, -apple-system, sans-serif';
      const vw = ctx.measureText(vStr).width;
      if (vw > v.h * 0.90) {
        fSizeVert = Math.max(3.5, fSizeVert * ((v.h * 0.90) / vw));
        ctx.font = 'bold ' + fSizeVert + 'px Inter, -apple-system, sans-serif';
      }
      ctx.fillStyle = '#000000';
      ctx.fillText(vStr, 0, 0);
      ctx.restore();
    }
  }
  
  // Banners Oficiales de Identificación de Módulos
  if (scale > 0.22) {
    ctx.save();
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    
    const moduleBanners = [
      { text: 'MÓDULO 12', x: 4294, y: 1420, w: 1554, h: 28, bg: '#FCE7F3', border: '#F472B6', col: '#831843' },
      { text: 'MÓDULO 7',  x: 5900, y: 865,  w: 748,  h: 28, bg: '#CFFAFE', border: '#06B6D4', col: '#164E63' },
      { text: 'MÓDULO 6',  x: 6788, y: 355,  w: 1412, h: 28, bg: '#D1FAE5', border: '#10B981', col: '#064E3B' },
      { text: 'MÓDULO 5',  x: 6788, y: 1505, w: 1412, h: 28, bg: '#D1FAE5', border: '#10B981', col: '#064E3B' },
      { text: 'MÓDULO 4',  x: 6788, y: 2020, w: 1412, h: 28, bg: '#D1FAE5', border: '#10B981', col: '#064E3B' },
      { text: 'MÓDULO 3',  x: 8320, y: 355,  w: 2243, h: 28, bg: '#E0F2FE', border: '#38BDF8', col: '#0C4A6E' },
      { text: 'MÓDULO 2',  x: 8320, y: 1505, w: 1186, h: 28, bg: '#FFEDD5', border: '#FB923C', col: '#7C2D12' },
      { text: 'MÓDULO 1',  x: 9596, y: 1505, w: 967,  h: 28, bg: '#FEF08A', border: '#FACC15', col: '#713F12' },
      { text: 'MÓDULO 13', x: 8320, y: 2655, w: 1186, h: 28, bg: '#E0E7FF', border: '#818CF8', col: '#312E81' },
      { text: 'MÓDULO 14', x: 9596, y: 2655, w: 967,  h: 28, bg: '#EDE9FE', border: '#A78BFA', col: '#4C1D95' },
      { text: 'MÓDULO 10', x: 4060, y: 5075, w: 1788, h: 28, bg: '#FFEDD5', border: '#FB923C', col: '#7C2D12' },
      { text: 'MÓDULO 11', x: 4790, y: 3165, w: 1058, h: 26, bg: '#86EFAC', border: '#22C55E', col: '#14532D' },
      { text: 'MÓDULO 9',  x: 4790, y: 4275, w: 1058, h: 26, bg: '#86EFAC', border: '#22C55E', col: '#14532D' }
    ];
    
    moduleBanners.forEach(mb => {
      if (mb.x + mb.w >= viewLeft && mb.x <= viewRight && mb.y + mb.h >= viewTop && mb.y <= viewBottom) {
        ctx.fillStyle = mb.bg;
        ctx.strokeStyle = mb.border;
        ctx.lineWidth = Math.max(1.5 / scale, 1.2);
        ctx.fillRect(mb.x, mb.y, mb.w, mb.h);
        ctx.strokeRect(mb.x, mb.y, mb.w, mb.h);
        ctx.fillStyle = mb.col;
        ctx.font = 'bold 13px Inter, -apple-system, sans-serif';
        ctx.fillText(mb.text, mb.x + mb.w / 2, mb.y + mb.h / 2);
      }
    });
    ctx.restore();
  }
  
  ctx.restore();
  ctx.restore();
}

function initMapaCanvas() {
  wrapper = document.getElementById('mapa-wrapper');
  canvas  = document.getElementById('mapaCanvasLectura');
  if (!wrapper || !canvas) return;
  ctx     = canvas.getContext('2d');
  tooltip = document.getElementById('mapa-tooltip');
  
  resizeCanvas();
  resetView(filtroModulo);
  
  if (canvas._hasMapaEvents) return;
  canvas._hasMapaEvents = true;
  
  window.addEventListener('resize', resizeCanvas);
  if (window.ResizeObserver && wrapper) {
    new ResizeObserver(() => {
      if (wrapper.offsetWidth > 0 && wrapper.offsetHeight > 0) {
        resizeCanvas();
      }
    }).observe(wrapper);
  }
  
  // Pan & Zoom
  canvas.addEventListener('mousedown', e => {
    if (e.button !== 0) return;
    isPanning = true;
    startX = e.clientX - panX;
    startY = e.clientY - panY;
  });
  
  window.addEventListener('mousemove', e => {
    if (isPanning) {
      panX = e.clientX - startX;
      panY = e.clientY - startY;
      render();
    } else if (canvas) {
      const rect = canvas.getBoundingClientRect();
      const mx = e.clientX - rect.left;
      const my = e.clientY - rect.top;
      if (mx >= 0 && mx <= rect.width && my >= 0 && my <= rect.height) {
        const wx = (mx - panX) / scale;
        const wy = (my - panY) / scale;
        const v = findValveAt(wx, wy);
        if (v !== hoveredValve) {
          hoveredValve = v;
          render();
        }
        if (v && tooltip) {
          const vid = String(v.id || "").trim().toUpperCase();
          const vcode = String(v.codigo || "").trim().toUpperCase();
          const hData = harvestMap.get(vid) || (vcode ? harvestMap.get(vcode) : null);
          const kgTot = (hData && hData.Kg_Total) ? Math.round(hData.Kg_Total) : 0;
          const haTot = (hData && hData.Ha_Cosechada) ? hData.Ha_Cosechada : 0;
          const rendVal = haTot > 0 ? Math.round(kgTot / haTot) : 0;
          
          let daysHtml = '';
          if (hData && hData.Pct_Total > 0) {
            DAY_DAYS.forEach(d => {
              const p = Number(hData[d.key]) || 0;
              const k = Number(hData[d.kgKey]) || 0;
              if (p > 0 || k > 0) {
                daysHtml += '<div style="display:flex; justify-content:space-between; gap:10px; margin-top:2px;">' +
                  '<span style="color:' + d.color + '; font-weight:700;">● ' + d.name + ':</span>' +
                  '<span>' + p + '% (' + k.toLocaleString('en-US') + ' kg)</span></div>';
              }
            });
          } else {
            daysHtml = '<div style="color:#94A3B8; font-style:italic; margin-top:3px;">Sin cosecha registrada esta semana</div>';
          }
          
          tooltip.innerHTML = '<div style="font-weight:700; font-size:13px; margin-bottom:2px; border-bottom:1px solid rgba(255,255,255,0.2); padding-bottom:3px;">' +
            v.codigo + ' <span style="font-weight:400; font-size:11.5px; opacity:0.85;">(' + (v.Variedad || '') + ')</span></div>' +
            '<div style="font-size:11px; opacity:0.85; margin-bottom:4px;">Módulo ' + (v.Módulo_Sub || v.Módulo) + ' • Turno ' + v.Turno + ' • Área: <b>' + v.Área + ' ha</b></div>' +
            (kgTot > 0 ? '<div style="display:flex; justify-content:space-between; gap:14px; margin-bottom:4px;"><span>Total Cosechado:</span><b style="color:#38BDF8;">' + kgTot.toLocaleString('en-US') + ' kg</b></div>' +
                         '<div style="display:flex; justify-content:space-between; gap:14px; margin-bottom:6px;"><span>Rendimiento:</span><b style="color:#4ADE80;">' + rendVal.toLocaleString('en-US') + ' kg/ha</b></div>' : '') +
            '<div style="border-top:1px solid rgba(255,255,255,0.15); padding-top:4px;">' + daysHtml + '</div>';
          tooltip.style.display = 'block';
          tooltip.style.left = Math.min(rect.width - 240, mx + 14) + 'px';
          tooltip.style.top  = Math.min(rect.height - 180, my + 14) + 'px';
        } else if (tooltip) {
          tooltip.style.display = 'none';
        }
      }
    }
  });
  
  window.addEventListener('mouseup', () => { isPanning = false; });
  
  canvas.addEventListener('wheel', e => {
    e.preventDefault();
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left;
    const my = e.clientY - rect.top;
    const zoom = e.deltaY < 0 ? 1.15 : 0.87;
    const newScale = Math.min(Math.max(scale * zoom, 0.05), 4.0);
    panX = mx - (mx - panX) * (newScale / scale);
    panY = my - (my - panY) * (newScale / scale);
    scale = newScale;
    render();
  }, { passive: false });
  
  const bZIn = document.getElementById('btn_mapa_zoom_in');
  if (bZIn) {
    bZIn.addEventListener('click', () => {
      const rect = canvas.getBoundingClientRect();
      const mx = rect.width / 2, my = rect.height / 2;
      const newScale = Math.min(scale * 1.25, 4.0);
      panX = mx - (mx - panX) * (newScale / scale);
      panY = my - (my - panY) * (newScale / scale);
      scale = newScale;
      render();
    });
  }
  
  const bZOut = document.getElementById('btn_mapa_zoom_out');
  if (bZOut) {
    bZOut.addEventListener('click', () => {
      const rect = canvas.getBoundingClientRect();
      const mx = rect.width / 2, my = rect.height / 2;
      const newScale = Math.max(scale * 0.80, 0.05);
      panX = mx - (mx - panX) * (newScale / scale);
      panY = my - (my - panY) * (newScale / scale);
      scale = newScale;
      render();
    });
  }
}

document.addEventListener('DOMContentLoaded', function() {
  setTimeout(initMapaCanvas, 150);
});

// Listener al alternar de tab para redimensionar y recentrar canvas
if (window.$) {
  $(document).on('shown.bs.tab shown.bs.pill', function(e) {
    const targetText = $(e.target).text() || '';
    const targetVal = $(e.target).attr('data-value') || '';
    if (targetText.includes('Ruta') || targetText.includes('cosecha') || targetText.includes('Mapa') || 
        targetVal === 'Ruta de cosecha' || targetVal === 'Mapa de Cosecha') {
      setTimeout(function() {
        initMapaCanvas();
        resizeCanvas();
        resetView();
      }, 100);
    }
  });
}

// Handlers de Shiny
if (window.Shiny) {
  Shiny.addCustomMessageHandler('actualizar_mapa_semana', function(data) {
    if (!data) return;
    initMapaCanvas();
    if (data.modulo_activo) { filtroModulo = data.modulo_activo; }
    
    if (data.ids_filtrados && Array.isArray(data.ids_filtrados) && data.ids_filtrados.length > 0) {
      filteredIds = new Set(data.ids_filtrados.map(id => String(id).trim().toUpperCase()));
    } else {
      filteredIds = null;
    }
    
    const k = data.kpis || {};
    const haEl = document.getElementById('mapa_kpi_ha');
    if (haEl) haEl.textContent = (k.ha_cosechada || 0).toLocaleString('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2}) + ' ha';
    
    const kgEl = document.getElementById('mapa_kpi_kg');
    if (kgEl) kgEl.textContent = Math.round(k.kg_total || 0).toLocaleString('en-US') + ' kg';
    
    const rendEl = document.getElementById('mapa_kpi_rend');
    if (rendEl) rendEl.textContent = Math.round(k.rend_promedio || 0).toLocaleString('en-US') + ' kg/ha';
    
    const dias = k.dias || {};
    const mapBadges = [
      { id: 'mapa_badge_lunes',     name: 'Lunes',     data: dias.Lunes },
      { id: 'mapa_badge_martes',    name: 'Martes',    data: dias.Martes },
      { id: 'mapa_badge_miercoles', name: 'Miércoles', data: dias.Miercoles },
      { id: 'mapa_badge_jueves',    name: 'Jueves',    data: dias.Jueves },
      { id: 'mapa_badge_viernes',   name: 'Viernes',   data: dias.Viernes },
      { id: 'mapa_badge_sabado',    name: 'Sábado',    data: dias.Sabado }
    ];
    mapBadges.forEach(b => {
      const el = document.getElementById(b.id);
      if (el) {
        const kgD = (b.data && b.data.kg) ? Math.round(b.data.kg).toLocaleString('en-US') : '0';
        const haD = (b.data && b.data.ha) ? b.data.ha.toFixed(2) : '0.00';
        el.textContent = '● ' + b.name + ': ' + kgD + ' kg (' + haD + ' ha)';
      }
    });
    
    buildHarvestMap(data.valvulas || []);
    resizeCanvas();
    resetView();
    requestAnimationFrame(function() {
      resizeCanvas();
      resetView();
    });
    setTimeout(function() {
      resizeCanvas();
      resetView();
    }, 60);
  });
  
  Shiny.addCustomMessageHandler('filtrar_modulo_mapa', function(mod) {
    filtroModulo = mod || 'Todos';
    resetView();
  });
  
  Shiny.addCustomMessageHandler('centrar_mapa_canvas', function() {
    resetView();
  });
}
