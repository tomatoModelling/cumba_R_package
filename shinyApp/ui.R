# shinyApp/ui.R ------------------------------------------------------------
# =====================================================================
# *** DOVE CAMBIARE I VALORI DI DEFAULT ALL'AVVIO DELL'APP ***
#
# 1. Strategia irrigua per fase (ws_veg/ws_rep/ws_rip + turn_*):
#    -> cerca 'phase_card("veg", ...' qui sotto (riga ~470).
#       Il 4o argomento e' la SOGLIA WS di default (0.50..1.00),
#       il 6o argomento e' il TURNO MINIMO in giorni (1..7).
#
# 2. Data trapianto:
#    -> cerca 'dateInput("transplantingDate", ..., value = Sys.Date())'.
#       Cambia 'Sys.Date()' con (es.) 'as.Date("2026-05-01")' per
#       fissare una data fissa.
#
# 3. Anni di storico:
#    -> cerca 'sliderInput(... "history_years" ..., value = 8)'.
#
# 4. Parametri di modello (RUE, Kc, fenologia, brix...):
#    -> sezione 'advancedPanel' qui sotto, ognuno ha 'value = ...'.
#
# 5. LLM (modello + endpoint):
#    -> shinyApp/global.R, variabili .LLM_BASE_URL e .LLM_MODEL.
#
# 6. Soglia "giorno piovoso" (default 10 mm):
#    -> shinyApp/global.R, costante .RAIN_DAY_MM.
# =====================================================================
#
# Layout a TRE STATI:
#  STATE = "pick"  -> hero pulito: CUMBA si presenta, mappa al centro,
#                     pulsante geolocalizza. Niente parametri, niente plot.
#  STATE = "focus" -> dashboard a TUTTA LARGHEZZA (default dopo aver
#                     scelto un sito). Mappa e strategia sono nascoste
#                     ma raggiungibili via toolbar (🗺 / ⚙).
#  STATE = "work"  -> mappa minimizzata in alto a sinistra, parametri sotto,
#                     dashboard a destra. Per quando l'utente vuole vedere
#                     entrambe le cose.
#  Lo stato e' controllato lato server via
#     session$sendCustomMessage("cumba_set_mode", ...)
#  che setta body[data-cumba-mode] e CSS fa il resto.

app_css <- "
  /* ---------- Typography & base ---------------------------------------- */
  body, .shiny-output-error, .control-label, .form-control, .selectize-input {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Inter',
                 Roboto, Helvetica, Arial, sans-serif;
  }
  body { background: #fafbfc; }
  .navbar { box-shadow: 0 1px 3px rgba(0,0,0,0.08); }

  /* ---------- App shell (hero / work) ---------------------------------- */
  .app-shell {
    display: flex; gap: 12px; padding: 8px;
    min-height: calc(100vh - 60px);
  }
  .left-col, .right-col { transition: all 0.35s ease; }

  /* === WORK MODE (mappa+strategia visibili, dashboard a destra) === */
  body[data-cumba-mode='work'] .left-col {
    flex: 0 0 360px; display: flex; flex-direction: column; gap: 6px;
  }
  body[data-cumba-mode='work'] .right-col {
    flex: 1 1 auto; display: flex; flex-direction: column; gap: 6px;
    min-width: 0;
  }
  body[data-cumba-mode='work'] .map-pane {
    height: 220px; position: relative; border-radius: 10px;
    overflow: hidden; box-shadow: 0 2px 6px rgba(0,0,0,0.08);
  }

  /* === FOCUS MODE (default dopo pick) ===
     Dashboard a tutta larghezza. Mappa e strategia nascoste.
     L'utente le richiama via la toolbar (toggle buttons in alto). */
  body[data-cumba-mode='focus'] .left-col { display: none; }
  body[data-cumba-mode='focus'] .right-col {
    flex: 1 1 100%; display: flex; flex-direction: column; gap: 6px;
    min-width: 0; max-width: 100%;
  }
  body[data-cumba-mode='focus'] .strategy-side { display: none; }

  /* === STRATEGY MODE: dashboard al centro + pannello strategia a destra. */
  body[data-cumba-mode='strategy'] .left-col { display: none; }
  body[data-cumba-mode='strategy'] .right-col {
    flex: 1 1 auto; display: flex; flex-direction: column; gap: 6px;
    min-width: 0;
  }
  body[data-cumba-mode='strategy'] .strategy-side {
    flex: 0 0 340px; display: flex; flex-direction: column; gap: 6px;
    background: #fff; padding: 10px; border-radius: 10px;
    border: 2px solid #ff7043;
    box-shadow: 0 4px 14px rgba(255,87,34,0.18);
    max-height: calc(100vh - 80px); overflow-y: auto;
    /* Round 10: sticky cosi' la strategia resta DI FIANCO al grafico
       anche quando l'utente scrolla la dashboard verso il basso. */
    position: sticky; top: 8px; align-self: flex-start;
  }
  body[data-cumba-mode='focus']  .strategy-side,
  body[data-cumba-mode='work']   .strategy-side,
  body[data-cumba-mode='pick']   .strategy-side { display: none !important; }

  /* === PICK MODE === */
  body[data-cumba-mode='pick'] .left-col {
    flex: 1 1 100%; max-width: 100%;
    position: relative; display: flex; flex-direction: column;
    align-items: center; padding: 0;
  }
  body[data-cumba-mode='pick'] .right-col { display: none; }
  body[data-cumba-mode='pick'] .map-pane {
    width: 100%; height: 78vh; position: relative;
    border-radius: 14px; overflow: hidden;
    box-shadow: 0 4px 20px rgba(0,0,0,0.10);
  }
  /* mostra solo in pick */
  body[data-cumba-mode='focus']    .pick-only,
  body[data-cumba-mode='work']     .pick-only,
  body[data-cumba-mode='strategy'] .pick-only { display: none !important; }
  /* mostra solo in work (sotto la mappa) */
  body[data-cumba-mode='pick']     .work-only,
  body[data-cumba-mode='focus']    .work-only,
  body[data-cumba-mode='strategy'] .work-only { display: none !important; }
  /* La toolbar di focus (toggle buttons) e' visibile in focus/work/strategy */
  body[data-cumba-mode='pick']     .focus-toolbar { display: none !important; }

  /* ---------- Focus toolbar (toggle map / strategy) -------------------- */
  .focus-toolbar {
    display: flex; gap: 6px; align-items: center;
    padding: 4px 6px; margin: 0 0 4px 0;
    background: linear-gradient(180deg, #f4f6f7 0%, #ffffff 100%);
    border: 1px solid #e0e3e5; border-radius: 8px;
  }
  .focus-toolbar .ft-title {
    font-size: 12px; color: #2e7d32; font-weight: 700;
    flex: 1; padding-left: 4px;
  }
  .focus-toolbar .btn-toggle {
    background: #fff !important; color: #455a64 !important;
    border: 1px solid #cfd5da !important;
    border-radius: 16px !important;
    padding: 3px 12px !important;
    font-size: 12px !important; font-weight: 600 !important;
  }
  .focus-toolbar .btn-toggle.active {
    background: #2e7d32 !important; color: #fff !important;
    border-color: #2e7d32 !important;
  }
  .focus-toolbar .btn-toggle:hover { background: #e8f5e9 !important; }
  /* Round 5: bottone Strategia molto piu' visibile (e' importante,
     l'agronomo ci entra spesso). Sfondo arancione vibrante. */
  .focus-toolbar .btn-strategy-hero {
    background: linear-gradient(135deg, #ff7043 0%, #ff5722 100%) !important;
    color: #fff !important;
    border: 2px solid #d84315 !important;
    border-radius: 18px !important;
    padding: 6px 18px !important;
    font-size: 13.5px !important; font-weight: 800 !important;
    box-shadow: 0 2px 6px rgba(255, 87, 34, 0.30);
    text-shadow: 0 1px 1px rgba(0,0,0,0.15);
    transition: all 0.18s;
  }
  .focus-toolbar .btn-strategy-hero:hover {
    background: linear-gradient(135deg, #ff5722 0%, #e64a19 100%) !important;
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(255, 87, 34, 0.45);
  }
  .focus-toolbar .btn-strategy-hero.active {
    background: linear-gradient(135deg, #d84315 0%, #bf360c 100%) !important;
    border-color: #bf360c !important;
  }
  /* Round 6: feedback IN ALTO — bordo verde piu' visibile, accento. */
  .irr-feedback.irr-feedback-top {
    border-left: 5px solid #00838f;
    background: linear-gradient(180deg, #ffffff 0%, #f7fcfd 100%);
  }
  .irr-feedback.irr-feedback-top .irrf-title {
    font-size: 14px; color: #006064;
  }
  /* Round 10: turno = input numerico inline (con +/- nativi del browser),
     sostituisce lo slider che e' sproporzionato per valori 1..7 interi. */
  .turn-input-row {
    display: flex; align-items: center; gap: 8px;
    padding: 4px 0;
  }
  .turn-input-row .turn-input-label {
    font-size: 11px; color: #455a64; font-weight: 600; flex: 1;
    margin: 0; line-height: 1.2;
  }
  .turn-input-row .form-group { margin: 0; }
  .turn-input-row input[type='number'] {
    text-align: center; font-size: 14px; font-weight: 700;
    padding: 4px 6px !important; height: 30px !important;
  }
  .turn-input-row .turn-input-unit {
    font-size: 11px; color: #6b7480;
  }

  /* Round 9: banner che annuncia che l'override agronomico e' ATTIVO
     sulla simulazione corrente. Sta sopra il grafico, ben visibile. */
  .override-banner {
    margin: 6px 0 4px 0; padding: 8px 14px;
    background: linear-gradient(90deg, #fff3e0 0%, #ffe0b2 100%);
    color: #bf360c; font-size: 13.5px; font-weight: 600;
    border-left: 4px solid #ef6c00; border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.06);
    line-height: 1.4;
  }

  /* Round 16: 4 KPI fenologici (fine trapianto, fior. inizio, max fior.,
     maturazione) sotto la today_card. Layout responsive flex-wrap. */
  .pheno-kpis-row {
    display: flex; gap: 6px; margin: 6px 0 0 0;
    flex-wrap: wrap;
  }
  .pheno-kpi {
    flex: 1 1 0; min-width: 100px;
    background: #fff; border-radius: 8px; padding: 6px 8px;
    border-left: 3px solid var(--pheno-color, #888);
    box-shadow: 0 1px 2px rgba(0,0,0,0.06);
    display: flex; align-items: center; gap: 6px;
    font-size: 11px;
  }
  .pheno-kpi.pheno-kpi-passed {
    opacity: 0.65;
    background: #f5f5f5;
  }
  .pheno-kpi-icon { font-size: 22px; line-height: 1; flex: 0 0 auto; }
  .pheno-kpi-body { flex: 1 1 auto; min-width: 0; }
  .pheno-kpi-label {
    font-size: 9.5px; color: #6b7480; font-weight: 600;
    text-transform: uppercase; letter-spacing: 0.4px;
    white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
    line-height: 1.1;
  }
  /* Round 19: layout 2-righe — data grossa sopra, fra X gg piccola sotto */
  .pheno-kpi-date {
    font-size: 14px; font-weight: 800; color: var(--pheno-color, #333);
    line-height: 1.15; margin-top: 1px;
    white-space: nowrap;
  }
  .pheno-kpi-delta {
    font-size: 10px; color: #6b7480; font-weight: 500;
    line-height: 1.2; white-space: nowrap;
  }
  .pheno-kpi.pheno-kpi-passed .pheno-kpi-delta { color: #999; }
  @media (max-width: 720px) {
    .pheno-kpi { flex: 1 1 calc(50% - 4px); }
  }

  /* Round 12: today card e forecast strip AFFIANCATI per risparmiare
     spazio verticale (la dashboard era troppo lunga). */
  .today-and-forecast {
    display: flex; gap: 8px; align-items: stretch;
    margin: 0 0 6px 0;
  }
  .today-and-forecast .today-side  { flex: 0 0 38%; min-width: 0; }
  .today-and-forecast .forecast-side { flex: 1 1 auto; min-width: 0; }
  .today-and-forecast .today-card { margin: 0; height: 100%; }
  .today-and-forecast .today-card .today-headline { font-size: 18px; }
  .today-and-forecast .today-card .today-icon { font-size: 36px; }
  .today-and-forecast .forecast-strip { padding: 6px; }
  .today-and-forecast .fc-day { min-width: 80px; }
  @media (max-width: 900px) {
    .today-and-forecast { flex-direction: column; }
    .today-and-forecast .today-side, .today-and-forecast .forecast-side { flex: 1 1 100%; }
  }

  /* Round 13: layout 2-col, schedule a SINISTRA (controllo) + grafico a
     DESTRA (impatto). Schedule super-compatto. */
  .plot-and-schedule {
    display: flex; gap: 8px; align-items: flex-start;
    margin: 6px 0 8px 0;
  }
  .plot-and-schedule.plot-and-schedule-inverted .irr-feedback-side {
    order: 0; border-left: none; border-right: 4px solid #00838f;
  }
  .plot-and-schedule.plot-and-schedule-inverted .plot-side { order: 1; }
  /* Round 20: mini-grafici PIU STRETTI per non rubare spazio al schedule */
  .plot-and-schedule .mini-charts-side {
    order: 2; flex: 0 0 130px; min-width: 0;
    background: #fff; border-radius: 8px;
    border: 1px solid #e0e3e5; padding: 2px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.06);
  }
  @media (max-width: 1280px) {
    .plot-and-schedule .mini-charts-side { display: none; }
  }
  .plot-and-schedule .plot-side { flex: 1 1 auto; min-width: 0; }
  .plot-and-schedule .irr-feedback-side {
    flex: 0 0 270px; max-height: 720px; overflow-y: auto;
    margin-top: 0;
  }
  /* Titolo + form aggiungi piu' compatti */
  .irr-feedback-side .irrf-title {
    font-size: 12px !important;
  }
  /* Round 13: collassabile per Save/Load/Reset (nascosto di default) */
  .irrf-storage-details {
    margin: 6px 0; font-size: 10.5px;
  }
  .irrf-storage-details > summary {
    cursor: pointer; padding: 4px 6px; background: #f5f5f5;
    border-radius: 4px; color: #555; user-select: none;
    list-style: none;
  }
  .irrf-storage-details > summary::-webkit-details-marker { display: none; }
  .irrf-storage-details > summary::before {
    content: '\25B8'; margin-right: 4px;
    transition: transform 0.15s;
  }
  .irrf-storage-details[open] > summary::before {
    content: '\25BE';
  }
  .irrf-storage-details .irrf-toolbar {
    flex-direction: column; align-items: stretch; gap: 4px;
    padding: 4px 0; border: none; margin: 0;
  }
  .irrf-storage-details .irrf-toolbar .btn,
  .irrf-storage-details .irrf-toolbar .shiny-input-container {
    width: 100% !important; font-size: 10.5px !important;
  }
  /* Round 18: bottone Ricalcola con regole, nello scheduling */
  .btn-recalc-rules {
    background: #e8f5e9 !important; color: #1b5e20 !important;
    border: 1px solid #66bb6a !important;
    font-weight: 600 !important; font-size: 11.5px !important;
    padding: 6px 10px !important; margin-bottom: 6px;
  }
  .btn-recalc-rules:hover { background: #c8e6c9 !important; }
  /* Round 18: row ghost-skip per skip su date che il modello non
     consiglia piu (perche ha ricalcolato). Pi opaca. */
  .irr-feedback .irrf-row.ghost {
    background: #fce4ec !important; opacity: 0.55;
  }
  /* Round 20: toggle freeze sopra al pulsante Ricalcola */
  .freeze-toggle-row {
    margin-bottom: 4px; padding: 4px 8px;
    background: #fff8e1; border-radius: 4px;
    border-left: 3px solid #ffb300;
  }
  .freeze-toggle-row .checkbox { margin: 0 !important; }
  .freeze-toggle-row label {
    font-size: 11px !important; font-weight: 600;
    color: #6d4c41 !important; line-height: 1.3;
  }
  /* Righe schedule ULTRA-COMPATTE */
  .irr-feedback-side .irrf-row {
    flex-wrap: nowrap; font-size: 10.5px; padding: 3px 5px;
    margin-bottom: 2px; align-items: center; gap: 3px;
  }
  .irr-feedback-side .irrf-date  { flex: 0 0 70px; font-size: 10px; line-height: 1.2; font-weight: 600; }
  .irr-feedback-side .irrf-mm    { flex: 0 0 38px; font-size: 11px; }
  .irr-feedback-side .irrf-tag   { display: none !important; }
  .irr-feedback-side .irrf-act   { font-size: 9.5px !important; padding: 2px 6px !important; }
  /* Form aggiungi: fields stacked dentro un fieldset arancione */
  .irr-feedback-side .add-row    { flex-direction: column; gap: 4px; align-items: stretch; }
  .irr-feedback-side .add-row-fields { display: flex; gap: 4px; }
  .irr-feedback-side .add-row-fields .form-group { flex: 1 1 auto; margin: 0; }
  .irr-feedback-side .add-row-top {
    background: #fff3e0; border: 1px solid #ffb74d;
    padding: 6px; border-radius: 6px; margin-bottom: 6px;
  }
  .irr-feedback-side .btn-add-irr {
    background: #ef6c00 !important; color: #fff !important;
    border: none !important; font-weight: 700 !important;
    padding: 6px 10px !important; width: 100%;
  }
  .irr-feedback-side .btn-add-irr:hover { background: #e65100 !important; }
  /* Mobile (<= 900px): 1 colonna, schedule sopra il grafico */
  @media (max-width: 900px) {
    .plot-and-schedule { flex-direction: column; }
    .plot-and-schedule .irr-feedback-side {
      flex: 1 1 100%; max-height: none;
      border-right: none; border-top: 4px solid #00838f;
    }
  }

  /* Round 7: feedback ATTACCATO al grafico (margin-top 0, bordo turchese
     piu' marcato sopra per dare l idea di continuita col plot). */
  .irr-feedback.irr-feedback-near {
    margin-top: 4px;
    border-top: 4px solid #00838f;
    background: linear-gradient(180deg, #ffffff 0%, #f7fcfd 100%);
    box-shadow: 0 2px 6px rgba(0,0,0,0.06);
  }
  .irr-feedback.irr-feedback-near .irrf-title {
    font-size: 13.5px; color: #006064; line-height: 1.35;
  }
  .irr-feedback .irrf-toolbar {
    display: flex; gap: 8px; align-items: center; flex-wrap: wrap;
    padding: 6px 0 8px 0;
    border-bottom: 1px dashed #e0e3e5; margin-bottom: 6px;
  }
  .irr-feedback .irrf-toolbar .form-group { margin: 0; }
  .irr-feedback .irrf-toolbar .btn { font-size: 11.5px !important; padding: 4px 10px !important; }
  .irr-feedback .irrf-toolbar .shiny-input-container { width: auto !important; }

  /* Round 5: bottone LLM trigger (apre il modal CUMBA ti spiega). */
  .focus-toolbar .btn-llm-trigger {
    background: #fff !important; color: #6d4c41 !important;
    border: 1.5px solid #bcaaa4 !important;
    border-radius: 18px !important;
    padding: 5px 14px !important;
    font-size: 12.5px !important; font-weight: 700 !important;
  }
  .focus-toolbar .btn-llm-trigger:hover {
    background: #efebe9 !important; color: #3e2723 !important;
  }
  .focus-toolbar .btn-llm-trigger.has-message {
    background: #fff8e1 !important; color: #6d4c41 !important;
    border-color: #ffb300 !important;
    animation: llm-pulse 2s ease-in-out infinite;
  }
  @keyframes llm-pulse {
    0%,100% { box-shadow: 0 0 0 0 rgba(255,179,0,0.45); }
    50%     { box-shadow: 0 0 0 8px rgba(255,179,0,0.00); }
  }

  /* Round 5: MODAL LLM in sovraimpressione (al click su .btn-llm-trigger).
     Backdrop semi-trasparente + card centrata. Niente plugin esterni:
     mostra/nasconde via classe .open su #llmModal. */
  #llmModal, .cumba-modal {
    position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
    background: rgba(0,0,0,0.55); z-index: 9000;
    display: none; align-items: center; justify-content: center;
    padding: 16px;
  }
  #llmModal.open, .cumba-modal.open { display: flex; }
  #llmModal .llm-modal-card, .cumba-modal .llm-modal-card {
    background: #fff; border-radius: 14px;
    max-width: 560px; width: 100%; max-height: 85vh;
    overflow-y: auto;
    box-shadow: 0 20px 60px rgba(0,0,0,0.35);
    padding: 18px 22px 16px 22px;
  }
  /* Round 6: il modal Avanzate va piu' largo (slider raggruppati) */
  .cumba-modal .adv-modal-card { max-width: 820px; }
  #llmModal .llm-modal-head {
    display: flex; align-items: center; gap: 10px;
    border-bottom: 1px solid #f0e8e3;
    padding-bottom: 8px; margin-bottom: 10px;
  }
  #llmModal .llm-modal-head .llm-icon { font-size: 26px; }
  #llmModal .llm-modal-head .llm-title {
    font-size: 17px; font-weight: 800; color: #6d4c41; flex: 1;
  }
  #llmModal .llm-modal-close {
    background: #fff; border: 1px solid #d7ccc8;
    border-radius: 50%; width: 30px; height: 30px;
    cursor: pointer; font-size: 14px; color: #6d4c41;
    line-height: 1;
  }
  #llmModal .llm-modal-close:hover { background: #efebe9; }
  #llmModal .llm-modal-body { font-size: 14px; line-height: 1.5; color: #3e2723; }
  #llmModal .llm-modal-controls {
    display: flex; gap: 8px; align-items: center;
    margin-top: 10px; padding-top: 10px;
    border-top: 1px dashed #f0e8e3;
  }

  /* Round 5: MOBILE responsive (smartphone < 720px wide).
     Tutto a colonna singola, font ridotto, padding compatto. */
  @media (max-width: 720px) {
    .app-shell { padding: 4px; gap: 6px; min-height: calc(100vh - 20px); }
    body[data-cumba-mode='focus']    .right-col,
    body[data-cumba-mode='strategy'] .right-col { flex: 1 1 100%; }
    body[data-cumba-mode='strategy'] .strategy-side {
      flex: 1 1 100% !important; max-height: none; overflow-y: visible;
    }
    body[data-cumba-mode='strategy'] .app-shell { flex-direction: column; }
    .focus-toolbar { flex-wrap: wrap; padding: 3px 5px; }
    .focus-toolbar .btn-toggle { font-size: 11.5px !important; padding: 3px 9px !important; }
    .focus-toolbar .btn-strategy-hero { font-size: 12.5px !important; padding: 5px 13px !important; }
    .site-header { padding: 8px 12px !important; }
    .today-card { padding: 10px 14px; gap: 10px; }
    .today-card .today-icon { font-size: 32px; }
    .today-card .today-headline { font-size: 17px; }
    .today-card .today-detail { font-size: 12px; }
    .irr-feedback .irrf-row { font-size: 11.5px; flex-wrap: wrap; }
    .irr-feedback .irrf-date { flex: 0 0 90px; }
    .irr-feedback .add-row { flex-direction: column; align-items: stretch; }
    .irr-feedback .add-row-fields { width: 100%; }
    .plot-wrap { min-height: 360px; }
    .forecast-strip { gap: 4px; padding: 6px; }
    #llmModal .llm-modal-card { max-width: 100%; max-height: 92vh; padding: 14px 16px; }
  }

  /* ---------- HERO overlay (pick mode) -------------------------------- */
  .hero-overlay {
    position: absolute; top: 28px; left: 50%;
    transform: translateX(-50%); z-index: 900;
    background: rgba(255,255,255,0.97);
    border-radius: 18px; padding: 22px 32px 18px 32px;
    text-align: center;
    box-shadow: 0 14px 44px rgba(0,0,0,0.18);
    border: 2px solid #c8e6c9;
    max-width: 620px; width: calc(100% - 40px);
  }
  .hero-overlay .hero-emoji  { font-size: 56px; line-height: 1; }
  .hero-overlay .hero-headline {
    font-size: 30px; font-weight: 800; color: #2e7d32;
    margin: 4px 0 2px 0; letter-spacing: -0.4px;
  }
  .hero-overlay .hero-sub {
    color: #6b7480; font-size: 14.5px; line-height: 1.4;
    margin-bottom: 14px;
  }
  .hero-overlay .hero-cta {
    font-size: 15px; font-weight: 700; color: #d84315;
    margin-bottom: 10px;
  }
  .hero-overlay .hero-or {
    margin: 10px 0 6px 0; color: #888; font-size: 11px;
    text-transform: uppercase; letter-spacing: 1.5px;
  }
  .hero-overlay .hero-hint {
    color: #1b5e20; font-weight: 600; font-size: 13.5px;
    background: #e8f5e9; padding: 6px 12px;
    border-radius: 20px; display: inline-block;
  }
  .hero-geoloc {
    font-size: 16px !important; font-weight: 700 !important;
    padding: 10px 22px !important; border-radius: 24px !important;
    box-shadow: 0 3px 8px rgba(46,125,50,0.30);
  }

  /* ---------- Map back-button (work mode) ----------------------------- */
  .map-back-btn {
    position: absolute; top: 8px; right: 8px; z-index: 800;
    background: rgba(255,255,255,0.92) !important;
    border: 1px solid #cfd5da !important;
    border-radius: 16px !important;
    padding: 3px 10px !important;
    font-size: 11px !important; font-weight: 600 !important;
    color: #455a64 !important;
    box-shadow: 0 1px 3px rgba(0,0,0,0.10);
  }
  .map-back-btn:hover {
    background: #fff !important; color: #d32f2f !important;
  }

  /* ---------- Params pane (work mode) --------------------------------- */
  .params-pane {
    background: #fff; padding: 8px; border-radius: 10px;
    border: 1px solid #e0e3e5;
    box-shadow: 0 1px 3px rgba(0,0,0,0.06);
    flex: 1 1 auto; overflow-y: auto; min-height: 0;
  }
  .params-bottom {
    margin-top: 10px; padding-top: 10px;
    border-top: 1px solid #eef0f2;
  }

  /* ---------- Section headers ----------------------------------------- */
  h4.section-header {
    font-size: 10.5px; font-weight: 700; letter-spacing: 0.6px;
    padding: 3px 8px; margin: 8px 0 4px 0;
    border-radius: 4px; text-transform: uppercase;
  }
  h4.growth-parameters    { color: #1b5e20; background-color: #c8e6c9; }
  h4.phenology-parameters { color: #0d47a1; background-color: #bbdefb; }
  h4.stress-parameters    { color: #b71c1c; background-color: #ffcdd2; }
  h4.brix-parameters      { color: #e65100; background-color: #ffe0b2; }

  /* ---------- Compact sliders ----------------------------------------- */
  .slider-container         { padding: 0 2px; margin: 0; }
  .slider-container .form-group { margin-bottom: 0; }
  .slider-container .control-label {
    font-size: 9.5px; margin: 0; padding: 0; line-height: 1.1;
    color: #555; font-weight: 500;
  }
  .slider-container .irs--shiny { height: 24px; min-height: 24px; margin-top: 0; }
  .slider-container .irs--shiny .irs-grid { display: none; }
  .slider-container .irs--shiny .irs-min,
  .slider-container .irs--shiny .irs-max { font-size: 8px; top: 0; padding: 0 2px; }
  .slider-container .irs--shiny .irs-line { top: 12px; height: 3px; }
  .slider-container .irs--shiny .irs-bar  { top: 12px; height: 3px; }
  .slider-container .irs--shiny .irs-from,
  .slider-container .irs--shiny .irs-to,
  .slider-container .irs--shiny .irs-single { font-size: 8.5px; padding: 0 3px; top: 0; }
  .slider-container .irs--shiny .irs-handle { top: 7px; width: 12px; height: 12px; }

  .growth-slider    .irs--shiny .irs-bar { background-color: #4CAF50; }
  .phenology-slider .irs--shiny .irs-bar { background-color: #1e88e5; }
  .stress-slider    .irs--shiny .irs-bar { background-color: #e53935; }
  .brix-slider      .irs--shiny .irs-bar { background-color: #fb8c00; }

  /* ---------- Phase cards (irrigation strategy) ------------------------ */
  .phase-card {
    background: #fff; border-radius: 8px; overflow: hidden;
    box-shadow: 0 1px 3px rgba(0,0,0,0.10);
    margin-bottom: 6px; border: 1px solid #e0e3e5;
  }
  .phase-card-header {
    padding: 5px 8px; color: #fff; font-weight: 700;
    font-size: 11px; letter-spacing: 0.3px; text-align: center;
    text-transform: uppercase;
  }
  .phase-card-header .phase-icon { font-size: 14px; margin-right: 4px; }
  .phase-card-header.veg { background: linear-gradient(135deg, #43a047, #2e7d32); }
  .phase-card-header.rep { background: linear-gradient(135deg, #1e88e5, #1565c0); }
  .phase-card-header.rip { background: linear-gradient(135deg, #fb8c00, #ef6c00); }
  .phase-card-body { padding: 4px 6px 6px 6px; }
  .phase-card-body .slider-container .control-label {
    font-size: 9px; color: #777;
  }
  .phase-card-body .irs--shiny .irs-bar { background-color: #00838f; }
  .phase-card-body .irs--shiny .irs-handle { border-color: #006064; }

  .strategy-title {
    font-size: 11px; font-weight: 700; color: #00838f;
    text-transform: uppercase; letter-spacing: 0.5px;
    margin: 6px 0 4px 0;
  }

  /* ---------- KPI cards ------------------------------------------------ */
  .kpi-row { display: flex; gap: 6px; flex-wrap: wrap; margin: 6px 0; }
  .kpi-card {
    flex: 1 1 110px; padding: 7px 9px;
    background: #fff; border-radius: 8px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.10);
    text-align: center; min-width: 95px;
    border: 1px solid #eef0f2;
  }
  .kpi-card .kpi-label {
    color: #6b7480; font-size: 9.5px;
    text-transform: uppercase; letter-spacing: 0.6px; font-weight: 600;
  }
  .kpi-card .kpi-value { font-size: 20px; font-weight: 700; color: #2e7d32; line-height: 1.05; }
  .kpi-card .kpi-unit  { color: #888; font-size: 9.5px; }
  .kpi-card .kpi-range {
    display: block; color: #6a1b9a; font-size: 9.5px;
    margin-top: 2px; font-weight: 600;
  }
  .kpi-card .kpi-vs    { display:block; color: #888; font-size: 9px; margin-top: 2px; font-weight: 500; }
  .kpi-card.irrig-events .kpi-value { color: #00838f; }

  /* ---------- Status bar ---------------------------------------------- */
  .status-bar {
    background: linear-gradient(180deg, #fff, #f4f6f7);
    padding: 6px 12px; border-radius: 6px;
    font-size: 12px; margin-bottom: 0;
    border: 1px solid #e0e3e5;
  }

  /* ---------- LLM card: collapsibile con <details> -------------------- */
  /* Quando chiusa: una barra sottile che mostra solo il titolo. */
  /* Quando aperta: pannello con il messaggio. Compatta in entrambi i casi.
     Posizione consigliata: IN FONDO al dashboard, larghezza ridotta. */
  details.llm-card {
    margin: 8px auto 8px auto;
    max-width: 720px;
    background: linear-gradient(135deg, #fff8e7 0%, #fffdf5 100%);
    border-left: 5px solid #f9a825;
    border-radius: 10px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.07);
    overflow: hidden;
  }
  details.llm-card > summary {
    list-style: none; cursor: pointer;
    padding: 7px 12px; display: flex;
    align-items: center; justify-content: space-between; gap: 10px;
    font-size: 12.5px; font-weight: 700; color: #6d4c41;
    user-select: none;
  }
  details.llm-card > summary::-webkit-details-marker { display: none; }
  details.llm-card > summary::after {
    content: '▾'; color: #8d6e63; font-size: 12px;
    transition: transform 0.2s ease;
  }
  details.llm-card[open] > summary::after { transform: rotate(180deg); }
  .llm-card .llm-body {
    padding: 4px 14px 10px 14px;
    border-top: 1px solid rgba(249, 168, 37, 0.3);
  }
  .llm-card .llm-controls {
    display: flex; align-items: center; gap: 10px;
    margin-bottom: 4px; padding-top: 6px;
  }
  .llm-card .llm-controls .form-group { margin: 0; }
  .llm-card .llm-controls .form-group .checkbox { margin: 0; }
  .llm-card .llm-text {
    margin-top: 4px;
    white-space: pre-wrap; line-height: 1.45;
    color: #3e2723; font-family: Georgia, serif; font-size: 13.5px;
    max-height: 200px; overflow-y: auto;
  }
  .llm-card .llm-status {
    margin-top: 6px; font-size: 11px; color: #8d6e63; font-style: italic;
    line-height: 1.35;
  }
  /* Indicatore di stato accanto al titolo */
  .llm-card .llm-pill {
    font-size: 10px; font-weight: 700; padding: 2px 8px; border-radius: 10px;
    margin-left: 6px; letter-spacing: 0.3px;
  }
  .llm-card .llm-pill.ok    { background: #c8e6c9; color: #1b5e20; }
  .llm-card .llm-pill.fb    { background: #ffe0b2; color: #bf360c; }
  .llm-card .llm-pill.err   { background: #ffcdd2; color: #b71c1c; }

  /* ---------- Phase badge (stato fenologico oggi) --------------------- */
  .phase-badge {
    display: inline-block; padding: 2px 10px; border-radius: 14px;
    background: #e8f5e9; color: #1b5e20; font-size: 11px;
    font-weight: 700; letter-spacing: 0.3px; margin-right: 6px;
    border: 1px solid #c8e6c9;
  }

  /* ---------- Date + suggerisci row ----------------------------------- */
  .transplant-row {
    display: flex; gap: 8px; align-items: flex-end;
  }
  .transplant-row .form-group { margin-bottom: 0; }
  .transplant-row .shiny-date-input { width: 100%; }
  .btn-suggest {
    background: #fff3e0 !important; color: #e65100 !important;
    border: 1px solid #ffcc80 !important;
    border-radius: 6px !important; font-weight: 700 !important;
    padding: 6px 10px !important; font-size: 12px !important;
    height: 34px;
  }
  .btn-suggest:hover { background: #ffe0b2 !important; }

  /* ---------- SITE HEADER (sito + stagione, molto visibile) ----------- */
  .site-header {
    display: flex; align-items: center; gap: 16px;
    padding: 10px 16px; margin: 0 0 6px 0;
    background: linear-gradient(135deg, #2e7d32 0%, #43a047 100%);
    color: #fff; border-radius: 10px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.12);
  }
  .site-header .site-pin { font-size: 28px; line-height: 1; }
  .site-header .site-where {
    flex: 1; min-width: 0;
  }
  .site-header .site-name {
    font-size: 18px; font-weight: 800; letter-spacing: -0.2px;
    line-height: 1.15;
    overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  }
  .site-header .site-coords {
    font-size: 11.5px; opacity: 0.85; letter-spacing: 0.3px;
  }
  .site-header .site-season {
    text-align: right; padding-left: 10px;
    border-left: 1px solid rgba(255,255,255,0.4);
  }
  /* Round 19: blocco tipo suolo nell header sito */
  .site-header .site-soil {
    display: flex; align-items: center; gap: 6px;
    padding: 0 10px; border-left: 1px solid rgba(255,255,255,0.4);
  }
  .site-header .site-soil .form-group { margin: 0; }
  .site-header .site-soil select {
    font-size: 12px !important; padding: 3px 6px !important;
    height: 26px !important;
  }
  .site-header .site-season .ss-label {
    font-size: 10.5px; text-transform: uppercase;
    letter-spacing: 0.7px; opacity: 0.85; margin-bottom: 4px;
  }
  /* Round 8: data trapianto inline nel header sito */
  .site-header .ss-trans-row {
    display: flex; align-items: center; gap: 6px;
  }
  .site-header .ss-trans-row .ss-trans-label {
    font-size: 12.5px; font-weight: 700; color: #fff; margin: 0;
  }
  .site-header .ss-trans-row .form-group { margin: 0; }
  .site-header .ss-trans-row input {
    font-size: 12px !important; padding: 3px 6px !important;
    height: 26px !important; border-radius: 4px;
  }
  .site-header .btn-suggest-inline {
    padding: 3px 8px !important; font-size: 13px !important;
    background: rgba(255,255,255,0.18) !important;
    border: 1px solid rgba(255,255,255,0.5) !important;
    color: #fff !important;
  }
  .site-header .btn-suggest-inline:hover {
    background: rgba(255,255,255,0.30) !important;
  }

  /* ---------- Sidebar strategia (mode = strategy) -------------------- */
  /* Round 17: bottone Ricalcola Strategia, prominente */
  .strategy-apply-row { margin: 4px 0 8px 0; }
  .strategy-apply-row .btn-apply-strategy {
    background: #2e7d32 !important; color: #fff !important;
    border: none !important; font-weight: 700 !important;
    padding: 8px 12px !important; font-size: 13px !important;
  }
  .strategy-apply-row .btn-apply-strategy:hover {
    background: #1b5e20 !important;
  }
  .strategy-dirty-hint {
    margin-top: 4px; padding: 4px 8px;
    background: #fff3e0; border-left: 3px solid #ef6c00;
    font-size: 11px; color: #bf360c; border-radius: 4px;
    animation: dirty-pulse 1.5s ease-in-out infinite;
  }
  @keyframes dirty-pulse {
    0%, 100% { background: #fff3e0; }
    50%      { background: #ffe0b2; }
  }

  .strategy-side h4.strategy-side-title {
    font-size: 13px; font-weight: 700; color: #00838f;
    text-transform: uppercase; letter-spacing: 0.4px;
    margin: 0 0 8px 0; padding: 0 4px;
  }
  .strategy-side .phase-card { margin-bottom: 10px; }
  .strategy-side .phase-card-header {
    font-size: 12px; padding: 7px 10px;
  }

  /* ---------- Irrigation feedback list ------------------------------- */
  .irr-feedback {
    background: #fff; border-radius: 10px;
    padding: 10px 12px; margin: 0 0 8px 0;
    border: 1px solid #e0e3e5;
    box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  }
  .irr-feedback .irrf-title {
    font-weight: 700; color: #00838f; font-size: 12.5px;
    margin-bottom: 6px;
  }
  .irr-feedback .irrf-row {
    display: flex; gap: 6px; align-items: center;
    padding: 5px 6px; border-radius: 6px;
    font-size: 12px; margin-bottom: 4px;
  }
  .irr-feedback .irrf-row.suggested  { background: #e0f7fa; }
  .irr-feedback .irrf-row.applied    { background: #e8f5e9; }
  .irr-feedback .irrf-row.skipped    { background: #fbe9e7; opacity: 0.75;
                                       text-decoration: line-through; }
  .irr-feedback .irrf-date  { flex: 0 0 110px; font-weight: 600; }
  .irr-feedback .irrf-mm    { flex: 0 0 60px;  color: #00838f; font-weight: 700; }
  .irr-feedback .irrf-tag   { flex: 1; font-size: 11px; color: #555; }
  .irr-feedback .irrf-actions { display: flex; gap: 4px; }
  .irr-feedback .irrf-act {
    font-size: 10.5px; padding: 3px 9px; line-height: 1.2;
    border-radius: 12px; border: 1px solid; background: #fff;
    cursor: pointer; font-weight: 600; transition: all 0.15s;
  }
  .irr-feedback .irrf-act.btn-skip   { color: #c62828; border-color: #ef9a9a; }
  .irr-feedback .irrf-act.btn-skip:hover   { background: #ffebee; }
  .irr-feedback .irrf-act.btn-undo   { color: #00838f; border-color: #80deea; }
  .irr-feedback .irrf-act.btn-undo:hover   { background: #e0f7fa; }
  .irr-feedback .irrf-act.btn-remove { color: #6d4c41; border-color: #d7ccc8;
                                       padding: 3px 7px; }
  .irr-feedback .irrf-act.btn-remove:hover { background: #efebe9; }
  .irr-feedback .irrf-row.empty {
    background: #f5f5f5; color: #777; justify-content: center;
  }
  .irr-feedback .add-row {
    display: flex; gap: 8px; align-items: flex-end;
    margin-top: 8px; padding-top: 8px;
    border-top: 1px dashed #e0e3e5;
  }
  .irr-feedback .add-row-fields {
    display: flex; gap: 6px; align-items: flex-end;
  }
  .irr-feedback .add-row .form-group { margin: 0; }
  .irr-feedback .add-row label {
    font-size: 10.5px; color: #777; font-weight: 600;
  }
  .irr-feedback .add-row .btn { white-space: nowrap; }

  /* ---------- TODAY HERO CARD (work mode) ----------------------------- */
  .today-card {
    margin: 0 0 8px 0; padding: 14px 18px;
    background: linear-gradient(135deg, #fff 0%, #fafdfa 100%);
    border-radius: 12px;
    border-left: 6px solid var(--today-color, #2e7d32);
    box-shadow: 0 2px 8px rgba(0,0,0,0.10);
    display: flex; align-items: center; gap: 16px;
  }
  .today-card .today-icon { font-size: 44px; line-height: 1; flex: 0 0 auto; }
  .today-card .today-body { flex: 1; }
  .today-card .today-date {
    font-size: 11px; text-transform: uppercase; letter-spacing: 0.6px;
    color: #6b7480; font-weight: 700;
  }
  .today-card .today-cycle {
    margin-left: 8px; padding: 2px 6px;
    background: #e8f5e9; color: #2e7d32; border-radius: 8px;
    font-size: 10.5px; font-weight: 700; letter-spacing: 0;
    text-transform: none;
  }
  .today-card .today-headline {
    font-size: 22px; font-weight: 800; color: var(--today-color, #2e7d32);
    margin: 2px 0 4px 0; line-height: 1.15;
  }
  .today-card .today-detail {
    font-size: 12.5px; color: #444; line-height: 1.4;
  }
  .vs-hist-text {
    margin: 0; padding: 8px 12px;
    background: #fff8e1; border-left: 4px solid #ffb300;
    border-radius: 4px; font-size: 12px; color: #5d4037;
    line-height: 1.4;
  }

  /* ---------- Next 3 days summary ------------------------------------- */
  .next3days {
    margin: 0; padding: 8px 12px;
    background: linear-gradient(90deg, #e3f2fd 0%, #fff 100%);
    border-left: 4px solid #1976d2; border-radius: 4px;
    font-size: 12.5px; color: #0d47a1; line-height: 1.45;
  }
  .next3days .next3days-title { font-weight: 700; color: #0d47a1; }
  .next3days .next3days-body  { color: #1a237e; }

  /* ---------- Forecast strip ------------------------------------------ */
  .fc-strip-header { margin: 0 0 4px 0; font-size: 12px; }
  .fc-strip-header .fc-strip-sub { color: #6b7480; margin-left: 8px; font-size: 11px; }
  .forecast-strip {
    display: flex; gap: 5px; overflow-x: auto;
    padding: 8px; background: linear-gradient(180deg, #f5f7f8, #ffffff);
    border-radius: 8px; margin-bottom: 0;
    border: 1px solid #e0e3e5;
  }
  .fc-day {
    flex: 0 0 auto; min-width: 88px; max-width: 110px;
    background: #fff; border-radius: 8px;
    padding: 7px 5px 6px 5px; text-align: center;
    box-shadow: 0 1px 2px rgba(0,0,0,0.06);
    font-size: 11px; border: 1px solid #eef0f2;
  }
  .fc-day.today      { border: 2px solid #2e7d32;
                       box-shadow: 0 2px 6px rgba(46,125,50,0.25); }
  .fc-day.transplant { border: 2px dashed #00838f; }
  .fc-day.irr-day    { background: linear-gradient(180deg,#e0f7fa 0%,#fff 50%); }
  .fc-day-name {
    font-size: 10px; color: #6b7480; text-transform: uppercase;
    letter-spacing: 0.4px; font-weight: 600;
  }
  .fc-date {
    font-weight: 700; color: #222; font-size: 12px;
    margin: 1px 0 1px 0;
  }
  .fc-icon { font-size: 22px; line-height: 1; margin: 3px 0; }
  .fc-stats { font-size: 9.5px; color: #555; line-height: 1.25; }
  .fc-stats div { margin: 0; }
  .fc-bigmm {
    font-size: 14px; font-weight: 800; color: #00838f;
    margin: 2px 0;
  }
  .fc-temp { font-size: 10px; color: #555; }
  .fc-prec { font-size: 9.5px; color: #1976d2; }
  .fc-advice {
    margin-top: 4px; padding: 3px 4px; border-radius: 4px;
    font-size: 10px; font-weight: 700; line-height: 1.2;
  }
  .advice-irrigate { background: #00838f; color: #fff; }
  .advice-ok       { background: #c8e6c9; color: #2e7d32; }
  .advice-rain     { background: #bbdefb; color: #0d47a1; }
  .advice-pre      { background: #f5f5f5; color: #888; }

  /* ---------- Plot wrapper ------------------------------------------- */
  .plot-wrap {
    background: #fff; border-radius: 8px; padding: 4px;
    border: 1px solid #e0e3e5;
    box-shadow: 0 1px 3px rgba(0,0,0,0.06);
  }
  .plot-empty {
    background: #fff; border-radius: 8px; padding: 36px 20px;
    border: 1px dashed #cfd5da;
    text-align: center; color: #6b7480;
    font-size: 13px;
  }
  .plot-empty .big { font-size: 32px; margin-bottom: 8px; }
"

# JS che gestisce mode-switch + geolocalizzazione browser ----------------
app_js <- "
  // Default mode al primo paint
  document.addEventListener('DOMContentLoaded', function(){
    if (!document.body.getAttribute('data-cumba-mode')) {
      document.body.setAttribute('data-cumba-mode', 'pick');
    }
  });
  // Toggle pick / focus / work
  Shiny.addCustomMessageHandler('cumba_set_mode', function(mode){
    document.body.setAttribute('data-cumba-mode', mode);
    // Resize per far ridimensionare leaflet/plotly al cambio layout
    setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 380);
  });
  // Aggiorna lo stato visivo dei toggle button (active class)
  Shiny.addCustomMessageHandler('cumba_set_toggle_active', function(payload){
    var ids = payload.ids || [];
    var active = payload.active || [];
    ids.forEach(function(id){
      var btn = document.getElementById(id);
      if (!btn) return;
      if (active.indexOf(id) >= 0) btn.classList.add('active');
      else btn.classList.remove('active');
    });
  });
  // Geolocalizzazione browser (HTML5)
  Shiny.addCustomMessageHandler('cumba_geolocate', function(_){
    if (!navigator.geolocation) {
      Shiny.setInputValue('geoloc_error',
        {msg: 'Geolocalizzazione non supportata.', ts: Date.now()},
        {priority: 'event'});
      return;
    }
    navigator.geolocation.getCurrentPosition(function(pos){
      Shiny.setInputValue('geoloc_result', {
        lng: pos.coords.longitude,
        lat: pos.coords.latitude,
        ts: Date.now()
      }, {priority: 'event'});
    }, function(err){
      Shiny.setInputValue('geoloc_error',
        {msg: err.message || 'permesso negato', ts: Date.now()},
        {priority: 'event'});
    }, {enableHighAccuracy: false, timeout: 12000, maximumAge: 60000});
  });
  // ---------- Feedback agricoltore (skip / undo) ----------
  // Event delegation: un solo handler per tutti i bottoni .irrf-act,
  // anche quelli generati dinamicamente da Shiny. data-act + data-date
  // descrivono l'azione (skip | undo-skip | remove-applied).
  document.addEventListener('click', function(ev){
    var t = ev.target.closest('.irrf-act');
    if (!t) return;
    ev.preventDefault();
    Shiny.setInputValue('irr_action', {
      act:  t.getAttribute('data-act'),
      date: t.getAttribute('data-date'),
      ts:   Date.now()
    }, {priority: 'event'});
  });
  // ---------- LLM modal + Advanced modal: apertura / chiusura ----------
  document.addEventListener('click', function(ev){
    // LLM
    if (ev.target.closest('#openLLM')) {
      var m = document.getElementById('llmModal');
      if (m) m.classList.add('open');
      Shiny.setInputValue('llm_modal_open',
        {open: true, ts: Date.now()}, {priority: 'event'});
      return;
    }
    if (ev.target.closest('#llmModalCloseBtn') ||
        ev.target.id === 'llmModal') {
      var m2 = document.getElementById('llmModal');
      if (m2) m2.classList.remove('open');
      Shiny.setInputValue('llm_modal_open',
        {open: false, ts: Date.now()}, {priority: 'event'});
      return;
    }
    // Advanced
    if (ev.target.closest('#openAdvanced')) {
      var ma = document.getElementById('advancedModal');
      if (ma) ma.classList.add('open');
      window.dispatchEvent(new Event('resize'));
      return;
    }
    if (ev.target.closest('#advancedModalCloseBtn') ||
        ev.target.id === 'advancedModal') {
      var ma2 = document.getElementById('advancedModal');
      if (ma2) ma2.classList.remove('open');
    }
  });
  document.addEventListener('keydown', function(ev){
    if (ev.key === 'Escape') {
      var ml = document.getElementById('llmModal');
      if (ml && ml.classList.contains('open')) {
        ml.classList.remove('open');
        Shiny.setInputValue('llm_modal_open',
          {open: false, ts: Date.now()}, {priority: 'event'});
      }
      var ma = document.getElementById('advancedModal');
      if (ma && ma.classList.contains('open'))
        ma.classList.remove('open');
    }
  });
  // Pulsa il bottone CUMBA quando arriva un nuovo messaggio (dal server)
  Shiny.addCustomMessageHandler('cumba_llm_pulse', function(on){
    var b = document.getElementById('openLLM');
    if (!b) return;
    if (on) b.classList.add('has-message');
    else    b.classList.remove('has-message');
  });
"

sl <- function(class_, ...) div(class = paste('slider-container', class_),
                                sliderInput(...))

# ---- Helper: a per-phase irrigation card --------------------------------
phase_card <- function(prefix, header_label, header_class,
                       ws_label, ws_default,
                       turn_label, turn_default, turn_max = 7L) {
  # Round 10: turno = numericInput con bottoni +/- (incrementi interi).
  # Soglia stress = slider (continuo 0.5..1).
  div(class = "phase-card",
    div(class = paste("phase-card-header", header_class), HTML(header_label)),
    div(class = "phase-card-body",
        sl("", paste0("ws_", prefix), ws_label,
           min = 0.5, max = 1.0, value = max(ws_default, 0.5), step = 0.05),
        # Turno: numericInput con +/- nativi del browser
        div(class = "turn-input-row",
            tags$label(turn_label, class = "turn-input-label"),
            numericInput(paste0("turn_", prefix), label = NULL,
                         value = min(turn_default, turn_max),
                         min = 1, max = turn_max, step = 1,
                         width = "70px"),
            tags$span(class = "turn-input-unit", "gg")))
  )
}

# ---- Quick (farmer) tab -------------------------------------------------
# NOTA: la STRATEGIA IRRIGUA (3 fasi) e la DATA TRAPIANTO sono ora gestite
# SOLO dal pannello laterale (.strategy-side, mode='strategy') per evitare
# duplicazione di input ID. Qui restano solo: anni di storico + lingua LLM.
quickPanel <- tabPanel(
  "Generale",
  div(class = "strategy-title", "📅 Contesto del campo"),
  fluidRow(
    column(12, sl("growth-slider", "history_years",
                  "Anni di storico", min = 3, max = 12, value = 8, step = 1))
  ),
  div(style = paste("margin-top:8px; padding:6px 10px;",
                    "background:#f3f5f7; border-left:3px solid #90a4ae;",
                    "border-radius:4px; font-size:11px; color:#455a64;",
                    "line-height:1.4;"),
      HTML(paste0("⚙ <strong>Strategia irrigua e data di trapianto:</strong> ",
                  "usa il bottone <strong>⚙ Strategia</strong> in alto.<br/>",
                  "<strong>Parametri di modello</strong> (RUE, Kc, fenologia, ",
                  "brix...): tab <strong>Avanzate</strong> qui sopra.")))
)

# ---- Advanced tab — 3-4 sliders per row --------------------------------
advancedPanel <- tabPanel(
  "Avanzate",
  h4("Crescita", class = "section-header growth-parameters"),
  fluidRow(
    column(6, sl("growth-slider", "TGro",   "T card. (°C)",  min = 6,  max = 36, value = c(10, 35))),
    column(6, sl("growth-slider", "Topt",   "T opt (°C)",    min = 22, max = 26, value = 24))
  ),
  fluidRow(
    column(6, sl("growth-slider", "TStress","T stress (°C)", min = 0,  max = 50, value = c(5, 45))),
    column(6, sl("growth-slider", "RUE",    "RUE (g/MJ)",    min = 1,  max = 3,  value = 2.8, step = 0.1))
  ),

  h4("Fenologia & canopy", class = "section-header phenology-parameters"),
  fluidRow(
    column(6, sl("phenology-slider", "CycleLength", "Cycle (°C·d)",
                 min = 1000, max = 1400, value = 1216, step = 10)),
    column(6, sl("phenology-slider", "LightInterception", "Light int.",
                 min = 0.001, max = 1, value = c(0.001, .9), step = 0.01))
  ),
  fluidRow(
    column(6, sl("phenology-slider", "TransFloLag", "Tran/Flow lag %",
                 min = 0, max = 50, value = c(9, 30))),
    column(6, sl("phenology-slider", "GrowthSenescenceCanopy", "Grow→Sen %",
                 min = 10, max = 130, value = c(19, 100.5)))
  ),
  fluidRow(
    column(6, sl("phenology-slider", "FloweringSlope", "Flow. slope",
                 min = 0.3, max = 0.6, value = 0.5, step = 0.01)),
    column(6, sl("phenology-slider", "FloweringMax",   "Flow. max",
                 min = 40, max = 90, value = 41))
  ),

  h4("Acqua & suolo", class = "section-header stress-parameters"),
  fluidRow(
    column(6, sl("stress-slider", "Kc",                     "Kc",
                 min = 0.2, max = 1.2, value = c(0.3, 1.15), step = 0.05)),
    column(6, sl("stress-slider", "RootIncrease",           "Root inc.",
                 min = 0.3,  max = .8, value = .42, step = 0.05))
  ),
  fluidRow(
    column(6, sl("stress-slider", "RootDepth",              "Root depth (cm)",
                 min = 1,    max = 100, value = c(4, 60))),
    column(6, sl("stress-slider", "WaterStressSensitivity", "WS sens.",
                 min = 0.5,  max = 4.5, value = 3.23, step = 0.1))
  ),
  fluidRow(
    column(6, sl("stress-slider", "DepletionFraction", "Depletion (% AWC)",
                 min = 20,   max = 80,  value = 30, step = 1)),
    column(6, sl("stress-slider", "SoilWaterInitial", "Init. soil W (%AWC)",
                 min = 30,   max = 100, value = 100, step = 5))
  ),

  h4("Qualita' frutto", class = "section-header brix-parameters"),
  fluidRow(
    column(6, sl("brix-slider", "k0",                            "k0 sugar",
                 min = 2,      max = 5,     value = 4, step = 0.1)),
    column(6, sl("brix-slider", "FruitWaterContent",             "Fruit water",
                 min = 0.8,    max = 1,     value = c(0.8, 0.91), step = 0.005))
  ),
  fluidRow(
    column(6, sl("brix-slider", "FruitWaterContentInc",          "Water inc.",
                 min = 0.05,  max = 0.2, value = 0.14,  step = 0.001)),
    column(6, sl("brix-slider", "FruitWaterContentDecreaseMax",  "Water dec. max",
                 min = 0.001, max = 0.0003, value = 0.0002, step = 0.0005))
  )
)

# ---- App ----------------------------------------------------------------
# Round 5: niente navbarPage / niente tabPanel — guadagniamo lo spazio della
# striscia in alto. fluidPage minimal con solo il nostro shell.
fluidPage(
  title = "CUMBA — l'amico agronomo del pomodoro",
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js)),
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1, maximum-scale=1")
  ),

    div(class = "app-shell",

      # ============== LEFT COLUMN: hero overlay + map + (work) params ===
      div(class = "left-col",

        # PICK MODE: hero overlay (nasconde tutto il dashboard)
        div(class = "hero-overlay pick-only",
            div(class = "hero-emoji", "🍅"),
            div(class = "hero-headline", "Ciao, sono CUMBA"),
            div(class = "hero-sub",
                "Sono il tuo amico agronomo per il pomodoro da industria. ",
                "Ti dico se irrigare, come va il campo, e cosa aspettarti ",
                "rispetto agli anni scorsi."),
            div(class = "hero-cta", "Dimmi dov'è il tuo campo:"),
            div(class = "hero-actions",
                actionButton("geolocBtn",
                             HTML("📍 Usa la mia posizione"),
                             class = "btn btn-success hero-geoloc")),
            div(class = "hero-or", "oppure"),
            div(class = "hero-hint", "👇 clicca un punto sulla mappa")
        ),

        # Mappa (sempre presente, dimensioni cambiano via CSS)
        div(class = "map-pane",
            leafletOutput("growthMap", height = "100%"),
            div(class = "work-only",
                actionButton("changeSite",
                             HTML("↺ cambia campo"),
                             class = "btn btn-default map-back-btn"))
        ),

        # Round 6: WORK MODE rimosso. Tutti i parametri (Generale + Avanzate)
        # sono ora nel modal #advancedModal apribile dalla toolbar in alto.
        # Il left-col contiene SOLO la mappa (in pick mode) + hero overlay.
        NULL
      ),

      # ============== RIGHT COLUMN: dashboard (focus + work + strategy) =
      div(class = "right-col",

        # Toolbar in alto: SOLO i toggle (titolo rimosso in Round 5 per
        # guadagnare spazio; il sito + stagione lo gestisce site_header sotto).
        # Il bottone Strategia ha uno stile dedicato "hero" (arancione) per
        # essere ben visibile: e' la leva agronomica principale dell'app.
        div(class = "focus-toolbar",
            actionButton("toggleMap",
                         HTML("🗺 Cambia sito"),
                         class = "btn btn-toggle",
                         title = "Torna alla vista mappa per scegliere un altro campo"),
            actionButton("toggleStrategy",
                         HTML("⚙ Strategia irrigua"),
                         class = "btn btn-toggle btn-strategy-hero",
                         title = "Apri il pannello con le 3 fasi (vegetativa, riproduttiva, maturazione)"),
            actionButton("openAdvanced",
                         HTML("🔧 Avanzate"),
                         class = "btn btn-toggle",
                         title = "Apri il pannello dei parametri di modello (RUE, Kc, fenologia, ...)"),
            actionButton("openLLM",
                         HTML("🤖 CUMBA ti spiega"),
                         class = "btn btn-toggle btn-llm-trigger",
                         title = "Apri la spiegazione di CUMBA in sovraimpressione")
        ),

        # *** HEADER: SITO + STAGIONE (molto visibile, in alto) ***
        uiOutput("site_header"),

        # ORDINE Round 12:
        # 1. Today card + forecast AFFIANCATI per risparmiare verticale
        # 2. KPI
        # 3. Grafici (sx) + Schedule (dx)
        div(class = "today-and-forecast",
            div(class = "today-side",
              shinycssloaders::withSpinner(
                uiOutput("today_card"),
                type = 6, color = "#2e7d32", proxy.height = "90px"
              ),
              # Round 16: 4 KPI fenologici subito sotto la today card
              uiOutput("pheno_kpis")
            ),
            div(class = "forecast-side",
              uiOutput("forecast_strip"))
        ),

        uiOutput("kpi_box"),

        # Round 14: banner override RIMOSSO (era confondente).

        # Round 13: layout INVERTITO -> schedule (pannello CONTROLLO) A
        # SINISTRA, grafico (pannello IMPATTO) A DESTRA. Cosi' l'utente
        # ha il flusso naturale "agisco -> vedo l'effetto a destra".
        div(class = "plot-and-schedule plot-and-schedule-inverted",
            div(class = "irr-feedback irr-feedback-side",
                div(class = "irrf-title",
                    HTML("💧 Le tue scelte di irrigazione")),
                # Round 20: toggle "modalita freeze". Quando ON, il modello
                # NON propone irrigazioni automatiche -> l'utente vede
                # l'impatto PURO del togliere un irrigazione.
                # Quando OFF, il modello compensa secondo la strategia.
                div(class = "freeze-toggle-row",
                    checkboxInput("freezeMode",
                                  HTML("🔒 Solo le mie scelte (no consigli automatici)"),
                                  value = FALSE)),
                # Round 18: bottone RICALCOLA in cima al pannello scheduling.
                actionButton("recalcWithRules",
                             HTML("🔄 Ricalcola con le regole della strategia"),
                             class = "btn btn-default btn-sm btn-recalc-rules",
                             width = "100%"),
                # Form AGGIUNGI in alto, ben visibile.
                div(class = "add-row add-row-top",
                    div(class = "add-row-fields",
                        dateInput("irr_apply_date", NULL,
                                  value = Sys.Date(), weekstart = 1L,
                                  width = "100%"),
                        numericInput("irr_apply_mm", NULL,
                                     value = 10, min = 1, max = 100, step = 1,
                                     width = "100%")),
                    actionButton("irr_apply_btn",
                                 "+ Aggiungi",
                                 class = "btn-primary btn-sm btn-add-irr")),
                # Round 13: Save/Load/Reset come icone compatte
                # (collassano in icone sole se non c'e' spazio)
                tags$details(class = "irrf-storage-details",
                  tags$summary(HTML("📁 Salva / Carica / Reset")),
                  div(class = "irrf-toolbar",
                      downloadButton("irr_export",
                                     "💾 Salva CSV",
                                     class = "btn btn-default btn-xs"),
                      fileInput("irr_import", NULL,
                                buttonLabel = "📂 Carica CSV",
                                accept = c(".csv", "text/csv"),
                                placeholder = "",
                                width = "100%"),
                      actionButton("irr_clear_overrides",
                                   "🧹 Reset",
                                   class = "btn btn-default btn-xs"))
                ),
                # Lista
                uiOutput("irrigation_feedback_rows")
            ),
            div(class = "plot-wrap plot-side",
                uiOutput("mainPlot_wrap")),
            # Round 19: mini-grafici yield+brix box storico vs corrente
            div(class = "mini-charts-side",
                plotlyOutput("mini_yield_brix", height = "640px"))
        ),
        NULL
      ),

      # ============== STRATEGY SIDEBAR (mode = strategy) ================
      # Pannello a destra che appare quando l'utente clicca "⚙ Strategia".
      # Mostra le 3 fasi UNA SOTTO L'ALTRA, ben visibili.
      div(class = "strategy-side",
          h4(class = "strategy-side-title", "💧 Strategia irrigua"),
          # Round 18: bottone APPLY rimosso (e' nello scheduling ora).
          # La strategia si applica direttamente quando muovi gli slider.
          # Round 10: default soglia 0.8 e turno 2 gg per TUTTE le fasi.
          phase_card("veg",  "🌱 Vegetativa",   "veg",
                     "Soglia stress (0=secco, 1=opt.)", 0.9,
                     "Turno minimo (giorni)",            2),
          phase_card("rep",  "🌸 Riproduttiva", "rep",
                     "Soglia stress (0=secco, 1=opt.)", 0.9,
                     "Turno minimo (giorni)",            2),
          phase_card("rip",  "🍅 Maturazione",  "rip",
                     "Soglia stress (0=secco, 1=opt.)", 0.9,
                     "Turno minimo (giorni)",            2),
          # Round 8: data trapianto e "💡 suggerisci" sono ora nel
          # site_header in alto (uiOutput("site_header") in server.R),
          # cosi' sono SUBITO settabili senza aprire il pannello strategia.
          NULL
      )
    ),

    # Round 5: MODAL "CUMBA ti spiega" — appare in sovraimpressione quando
    # l'utente clicca il bottone .btn-llm-trigger in alto. Aggiungere/togliere
    # la classe .open su #llmModal lo mostra/nasconde (vedi app_js).
    div(id = "llmModal",
        div(class = "llm-modal-card",
            div(class = "llm-modal-head",
                span(class = "llm-icon", "🤖"),
                span(class = "llm-title",
                     HTML("CUMBA ti spiega"),
                     uiOutput("llm_pill", inline = TRUE)),
                tags$button(class = "llm-modal-close",
                            type = "button",
                            id = "llmModalCloseBtn",
                            HTML("✕"))),
            div(class = "llm-modal-body",
                shinycssloaders::withSpinner(
                  div(class = "llm-text", uiOutput("interpretation_text")),
                  type = 8, color = "#6d4c41", proxy.height = "60px"
                ),
                uiOutput("llm_status")
            ),
            div(class = "llm-modal-controls",
                checkboxInput("autoLLM", "Auto-aggiorna", value = TRUE),
                actionButton("interpretBtn",
                             "↻ Riprova",
                             class = "btn-primary btn-sm"),
                selectInput("language", NULL,
                            choices = c("Italiano" = "italiano",
                                        "Foggiano" = "foggiano",
                                        "English"  = "english"),
                            width = "130px"))
        )
    ),

    # Round 6: MODAL "Avanzate" — UNICA destinazione di TUTTI i parametri
    # di modello (Generale + Avanzate). Sostituisce il vecchio params-pane
    # del work-mode. Aperto da .openAdvanced in toolbar.
    div(id = "advancedModal", class = "cumba-modal",
        div(class = "llm-modal-card adv-modal-card",
            div(class = "llm-modal-head",
                span(class = "llm-icon", "🔧"),
                span(class = "llm-title", "Parametri di modello"),
                tags$button(class = "llm-modal-close",
                            type = "button",
                            id = "advancedModalCloseBtn",
                            HTML("✕"))),
            div(class = "llm-modal-body",
                tabsetPanel(
                  id = "param_tabs", type = "tabs",
                  quickPanel,
                  advancedPanel
                ),
                tags$hr(),
                actionButton("forceRun", "↻ ricalcola",
                             icon = icon("rotate"),
                             class = "btn-success btn-sm",
                             width = "100%")
            )
        )
    )
)
