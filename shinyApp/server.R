# shinyApp/server.R --------------------------------------------------------
# Reactive graph (with caching for historical runs):
#
#   selected_point + history_years + forceRun  ───►  weather_data
#                                                       │
#                            ┌──────────────────────────┤
#                            ▼                          ▼
#                 historical_runs                  current_run
#                 (isolate params:                 (sliders trigger
#                  re-runs only if                  re-run, debounced)
#                  weather changes)
#                            │                          │
#                            ▼                          │
#                       hist_envelope                   │
#                       (P10/P50/P90 by DOY,            │
#                        mapped on current year)        │
#                            └────────────┬─────────────┘
#                                         ▼
#                       status / KPI / forecast strip /
#                       always-on plot (historical baseline +
#                       current run overlay) / LLM

function(input, output, session) {

  # ===== Map ==============================================================
  output$growthMap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     attributionControl = FALSE)) |>
      addProviderTiles("CartoDB.Voyager",   group = "Map") |>
      addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
      addLayersControl(baseGroups = c("Map", "Satellite"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      setView(lng = 15.5, lat = 41.5, zoom = 6) |>
      addSearchOSM(options = searchOptions(collapsed = TRUE,
                                           zoom = 9, autoCollapse = TRUE)) |>
      addScaleBar(position = "bottomleft")
  })

  # ===== Selected site ====================================================
  selected_point <- reactiveVal(NULL)
  observeEvent(input$growthMap_click, {
    cl <- input$growthMap_click
    selected_point(c(cl$lng, cl$lat))
  })
  observeEvent(input$growthMap_search_marker, {
    m <- input$growthMap_search_marker
    selected_point(c(m$lng, m$lat))
  })
  # Stato della UI: pick (iniziale) / focus (dashboard solo) / work (mappa+strategia+dashboard)
  ui_mode <- reactiveVal("pick")
  set_mode <- function(m) {
    ui_mode(m)
    session$sendCustomMessage("cumba_set_mode", m)
    # Sincronizza i bottoni della toolbar (.active).
    # toggleMap e' "active" solo quando siamo in pick; toggleStrategy quando in strategy.
    active_ids <- character()
    if (m == "strategy") active_ids <- c(active_ids, "toggleStrategy")
    session$sendCustomMessage("cumba_set_toggle_active",
                              list(ids = c("toggleMap", "toggleStrategy"),
                                   active = active_ids))
  }

  # Reverse geocoding (Nominatim) — mostra il nome del luogo dopo il clic.
  place_name <- reactiveVal(NULL)
  observeEvent(selected_point(), {
    pt <- selected_point()
    if (is.null(pt) || length(pt) != 2) return()
    leafletProxy("growthMap") |>
      clearGroup("pick") |>
      addCircleMarkers(lng = pt[1], lat = pt[2], group = "pick",
                       color = "#d32f2f", radius = 9, weight = 2,
                       fillOpacity = 0.45,
                       popup = sprintf("📍 %.3f°N, %.3f°E", pt[2], pt[1])) |>
      flyTo(lng = pt[1], lat = pt[2], zoom = 9)
    # Passaggio pick -> focus (dashboard a tutta larghezza, default)
    set_mode("focus")
    # Reverse-geocode in background
    place_name(NULL)
    nm <- tryCatch(reverse_geocode_nominatim(pt[2], pt[1]),
                   error = function(e) NULL)
    if (!is.null(nm) && nzchar(nm)) place_name(nm)
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # Bottone "cambia campo" -> torna a pick
  observeEvent(input$changeSite, {
    selected_point(NULL)
    place_name(NULL)
    leafletProxy("growthMap") |> clearGroup("pick")
    set_mode("pick")
  })

  # I due bottoni della toolbar fanno cose DISTINTE:
  #   🗺 Mappa     -> torna alla vista pick (selezione di un altro sito).
  #                   Niente dashboard, mappa fullscreen.
  #   ⚙ Strategia -> apre la sidebar laterale con le 3 fasi una sotto l'altra.
  #                   Riclic = chiudi (torna a focus).
  observeEvent(input$toggleMap, {
    # Salviamo il punto: l'utente puo' poi clic-cambiarlo.
    set_mode("pick")
  })
  observeEvent(input$toggleStrategy, {
    set_mode(if (ui_mode() == "strategy") "focus" else "strategy")
  })

  # ===== Feedback agricoltore: skip / aggiungi irrigazioni ================
  # irr_overrides() e' una named-list keyed-by-date (chr "YYYY-MM-DD") che
  # registra cosa l'agricoltore ha effettivamente fatto vs il consiglio del
  # modello:
  #   list(action = "skip")               -> consiglio NON eseguito
  #   list(action = "applied", mm = 12)   -> irrigazione fatta in piu' (o
  #                                          al posto del consiglio)
  # Le override NON re-eseguono il modello: vengono solo SOVRAPPOSTE sul
  # plot e usate per generare un summary onesto verso il LLM ("modello
  # consigliava 8 mm il 14/05; agricoltore non ha irrigato"). Ri-runnare il
  # modello con irrigazioni custom richiederebbe un argomento dedicato in
  # cumba_scenario; lo faremo dopo.
  irr_overrides <- reactiveVal(list())

  # Singolo handler che processa TUTTI i click sui bottoncini .irrf-act
  # (skip / undo-skip / remove-applied) inviati via custom JS
  observeEvent(input$irr_action, {
    a <- input$irr_action
    if (is.null(a) || is.null(a$act) || is.null(a$date)) return()
    key <- as.character(a$date)
    ovr <- irr_overrides()
    if (a$act == "skip") {
      ovr[[key]] <- list(action = "skip")
    } else if (a$act == "undo-skip") {
      ovr[[key]] <- NULL
    } else if (a$act == "remove-applied") {
      ovr[[key]] <- NULL
    }
    irr_overrides(ovr)
  }, ignoreInit = TRUE)

  # Aggiunta manuale (form a fondo lista): data + mm
  observeEvent(input$irr_apply_btn, {
    d <- input$irr_apply_date
    mm <- suppressWarnings(as.numeric(input$irr_apply_mm))
    if (is.null(d) || is.na(d) || !is.finite(mm) || mm <= 0) {
      showNotification("Indica data + mm > 0 per registrare l'irrigazione.",
                       type = "warning", duration = 4)
      return()
    }
    key <- format(as.Date(d), "%Y-%m-%d")
    ovr <- irr_overrides()
    ovr[[key]] <- list(action = "applied", mm = round(mm, 1))
    irr_overrides(ovr)
    showNotification(sprintf("✓ Registrata: %.1f mm il %s",
                             mm, format(as.Date(d), "%d %b")),
                     type = "default", duration = 3)
  })

  # ===== Geolocalizzazione browser ========================================
  observeEvent(input$geolocBtn, {
    session$sendCustomMessage("cumba_geolocate", list())
  })
  observeEvent(input$geoloc_result, {
    r <- input$geoloc_result
    if (!is.null(r$lng) && !is.null(r$lat)) {
      selected_point(c(as.numeric(r$lng), as.numeric(r$lat)))
      showNotification(sprintf("📍 Trovato: %.3f°N, %.3f°E",
                               as.numeric(r$lat), as.numeric(r$lng)),
                       type = "default", duration = 3)
    }
  })
  observeEvent(input$geoloc_error, {
    showNotification(paste("Geolocalizzazione fallita:",
                           input$geoloc_error$msg,
                           "— prova a cliccare sulla mappa."),
                     type = "warning", duration = 6)
  })

  # ===== Suggerisci data trapianto (no pioggia 2 gg, no gelate) ===========
  observeEvent(input$suggestTransplant, {
    om <- weather_data()
    if (is.null(om) || !nrow(om)) {
      showNotification("Nessun dato meteo disponibile — clicca prima sulla mappa.",
                       type = "warning", duration = 5)
      return()
    }
    d <- suggest_transplanting_date(om, today = Sys.Date(),
                                    window_days = 30L, max_rain_2d = 1)
    if (is.na(d)) {
      showNotification(
        paste("Nei prossimi 30 giorni non trovo una finestra ideale",
              "(senza pioggia + abbastanza calda). Prova a scegliere a mano."),
        type = "warning", duration = 6)
      return()
    }
    updateDateInput(session, "transplantingDate", value = d)
    showNotification(
      sprintf("💡 Data suggerita: %s — finestra senza pioggia e suolo caldo.",
              format(d, "%a %d %B")),
      type = "default", duration = 6)
  })

  # ===== Trapianto: reactive DOY <- date dell'utente ======================
  # L'agricoltore manipola una data; convertiamo in DOY per CUMBA.
  transplantingDOY <- reactive({
    d <- input$transplantingDate
    if (is.null(d) || length(d) == 0L) return(120L)
    as.integer(format(as.Date(d), "%j"))
  })

  # ===== Weather (Open-Meteo: archive + forecast) =========================
  weather_data <- eventReactive(
    list(selected_point(), input$history_years, input$forceRun),
    {
      pt <- selected_point()
      req(pt, length(pt) == 2)
      today  <- Sys.Date()
      cur_yr <- as.integer(format(today, "%Y"))
      hyears <- if (is.null(input$history_years)) 8L else input$history_years
      start  <- as.Date(sprintf("%d-01-01", cur_yr - hyears))
      end    <- today + 16L
      withProgress(message = "🌦️ Open-Meteo (archive + forecast)…",
                   value = 0.3, {
        om <- tryCatch(
          fetch_openmeteo(pt[1], pt[2], start, end),
          error = function(e) {
            showNotification(paste("Open-Meteo:", conditionMessage(e)),
                             type = "error", duration = 8)
            NULL
          }
        )
        setProgress(1)
        om
      })
    },
    ignoreNULL = TRUE
  )

  # ===== Parameters (debounced) ===========================================
  params_raw <- reactive({ build_param_df(input) })
  params_df  <- params_raw |> debounce(350)

  # ===== Run cumba safely for one year ====================================
  run_one_year <- function(weather_year, par, transplantingDOY,
                           ws_v, ws_r, ws_p, t_v, t_r, t_p) {
    if (is.null(weather_year) || !nrow(weather_year)) return(NULL)
    tryCatch({
      res <- cumba_scenario(
        weather            = weather_year,
        param              = par,
        estimateRad        = TRUE,
        estimateET0        = TRUE,
        transplantingDOY   = transplantingDOY,
        irrigationStrategy = list(
          vegetative   = list(wsLevel = ws_v, turnMin = t_v),
          reproductive = list(wsLevel = ws_r, turnMin = t_r),
          ripening     = list(wsLevel = ws_p, turnMin = t_p)
        ),
        fullOut            = TRUE
      )
      if (is.null(res) || !nrow(res)) return(NULL)
      res$DATE <- as.Date(paste(res$year, res$doy, sep = "-"), format = "%Y-%j")
      res
    }, error = function(e) NULL)
  }

  # ===== Historical runs ==================================================
  # CACHED: triggers only when weather_data changes (i.e. on
  # site / years / forceRun). Slider parameter changes do NOT re-run
  # the historical batch — only the current year is re-simulated.
  historical_runs <- reactive({
    om <- weather_data();  req(om)

    # Snapshot params at this moment (no reactive dependency on them)
    par     <- isolate(params_df())
    trDOY   <- isolate(transplantingDOY())
    ws_v    <- isolate(input$ws_veg);  ws_r <- isolate(input$ws_rep);  ws_p <- isolate(input$ws_rip)
    t_v     <- isolate(input$turn_veg); t_r <- isolate(input$turn_rep); t_p <- isolate(input$turn_rip)
    req(par)

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    past_years <- sort(unique(om_cumba$year[om_cumba$year < cur_yr]))
    if (!length(past_years)) return(NULL)

    withProgress(message = "📊 Storico — simulazione 1 run/anno…", value = 0, {
      n <- length(past_years)
      all <- lapply(seq_along(past_years), function(i) {
        y <- past_years[i]
        setProgress(i / n, detail = sprintf("anno %d/%d", i, n))
        wy <- om_cumba[om_cumba$year == y, , drop = FALSE]
        run_one_year(wy, par, trDOY, ws_v, ws_r, ws_p, t_v, t_r, t_p)
      })
      setProgress(1)
      all <- Filter(Negate(is.null), all)
      if (!length(all)) return(NULL)
      do.call(rbind, all)
    })
  })

  # ===== Current-season run (re-fires on slider changes too) ==============
  current_run <- reactive({
    om  <- weather_data();  req(om)
    par <- params_df();     req(par)

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    wy <- om_cumba[om_cumba$year == cur_yr, , drop = FALSE]

    out <- run_one_year(wy, par, transplantingDOY(),
                        input$ws_veg, input$ws_rep, input$ws_rip,
                        input$turn_veg, input$turn_rep, input$turn_rip)
    if (is.null(out)) return(NULL)

    fc_lookup <- om[, c("DATE", "is_forecast")]
    fc_lookup$DATE <- as.Date(fc_lookup$DATE)
    out <- dplyr::left_join(out, fc_lookup, by = "DATE")
    out$is_forecast[is.na(out$is_forecast)] <- FALSE
    out
  })

  # ===== Ensemble proiezione (analoghi storici) ==========================
  # Costruisce N "scenari di continuazione" della stagione corrente:
  #   prefisso comune  : meteo cur_yr (osservato + forecast Open-Meteo +16d)
  #   coda variabile   : meteo dell'anno passato y rimappato a cur_yr,
  #                      a partire dal giorno (oggi + 17).
  # Output: data.frame "lungo" con colonna template_year. Tutti i membri
  # sono IDENTICI fino a today+16 e divergono dopo: e' la base del
  # "ventaglio" che il fattore richiede.
  current_run_ensemble <- reactive({
    om  <- weather_data();  req(om)
    par <- params_df();     req(par)

    cur_yr      <- as.integer(format(Sys.Date(), "%Y"))
    today       <- Sys.Date()
    cutoff      <- today + 16L
    cutoff_doy  <- as.integer(format(cutoff, "%j"))

    om_cumba <- om_to_cumba(om)
    om_cumba$year <- lubridate::year(om_cumba$DATE)
    cur_w <- om_cumba[om_cumba$year == cur_yr & om_cumba$DATE <= cutoff, ,
                      drop = FALSE]
    if (!nrow(cur_w)) return(NULL)

    past_years <- sort(unique(om_cumba$year[om_cumba$year < cur_yr]))
    if (length(past_years) < 2L) return(NULL)

    withProgress(message = "🎲 Ensemble (analoghi storici)…", value = 0, {
      n <- length(past_years)
      runs <- lapply(seq_along(past_years), function(i) {
        y <- past_years[i]
        setProgress(i / n, detail = sprintf("analogo %d/%d (%d)", i, n, y))
        tail_w <- om_cumba[om_cumba$year == y, , drop = FALSE]
        if (!nrow(tail_w)) return(NULL)
        tail_w$.doy <- as.integer(format(tail_w$DATE, "%j"))
        tail_w <- tail_w[tail_w$.doy > cutoff_doy, , drop = FALSE]
        if (!nrow(tail_w)) return(NULL)
        # Rimappa la coda all'anno corrente (stesso DOY)
        tail_w$DATE <- as.Date(sprintf("%d-%03d", cur_yr, tail_w$.doy),
                               format = "%Y-%j")
        tail_w$year <- cur_yr
        tail_w$.doy <- NULL
        stitched <- dplyr::bind_rows(cur_w, tail_w)
        stitched <- stitched[!duplicated(stitched$DATE), ]
        stitched <- stitched[order(stitched$DATE), ]
        out <- run_one_year(stitched, par, transplantingDOY(),
                            input$ws_veg, input$ws_rep, input$ws_rip,
                            input$turn_veg, input$turn_rep, input$turn_rip)
        if (is.null(out)) return(NULL)
        out$template_year <- y
        out
      })
      setProgress(1)
      runs <- Filter(Negate(is.null), runs)
      if (!length(runs)) return(NULL)
      do.call(rbind, runs)
    })
  }) |> debounce(800)

  # Envelope ensemble (P10/P50/P90 per DOY) per il "fan" sul plot.
  current_run_envelope <- reactive({
    ens <- current_run_ensemble()
    if (is.null(ens) || !nrow(ens)) return(NULL)
    cols <- intersect(c("fIntAct","brixAct","fruitFreshWeightAct","ftsw",
                        "waterStress","irrigation"), names(ens))
    if (!length(cols)) return(NULL)
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    ens |>
      group_by(doy) |>
      summarise(across(all_of(cols),
                       list(p10 = ~quantile(., 0.10, na.rm = TRUE),
                            p50 = ~quantile(., 0.50, na.rm = TRUE),
                            p90 = ~quantile(., 0.90, na.rm = TRUE)),
                       .names = "{.col}_{.fn}"),
                .groups = "drop") |>
      mutate(DATE = as.Date(sprintf("%d-%03d", cur_yr, doy),
                            format = "%Y-%j"))
  })

  # ===== Historical envelope (mapped onto current year calendar) ==========
  hist_envelope <- reactive({
    h <- historical_runs()
    if (is.null(h) || !nrow(h)) return(NULL)

    cols <- intersect(c("fIntAct", "waterStress", "heatStress", "coldStress",
                        "brixAct", "fruitsStateAct", "fruitFreshWeightAct",
                        "irrigation", "p", "tMax", "tMin", "ftsw"),
                      names(h))
    if (!length(cols)) return(NULL)

    env <- h |>
      group_by(doy) |>
      summarise(across(all_of(cols),
                       list(p10 = ~quantile(., 0.10, na.rm = TRUE),
                            p50 = ~quantile(., 0.50, na.rm = TRUE),
                            p90 = ~quantile(., 0.90, na.rm = TRUE)),
                       .names = "{.col}_{.fn}"),
                .groups = "drop")

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    env$DATE <- as.Date(sprintf("%d-01-01", cur_yr)) + (env$doy - 1L)
    env
  })

  # ===== SITE HEADER (sito + stagione, molto visibile) ===================
  output$site_header <- renderUI({
    pt <- selected_point()
    if (is.null(pt)) return(NULL)   # in pick mode l'header non serve

    nm <- place_name()
    coords <- sprintf("%.3f°N, %.3f°E", pt[2], pt[1])
    where_name <- if (!is.null(nm) && nzchar(nm)) nm else "Campo selezionato"

    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    trans_date <- as.Date(input$transplantingDate %||% Sys.Date())
    trans_str  <- format(trans_date, "%d %b")

    div(class = "site-header",
        div(class = "site-pin", "📍"),
        div(class = "site-where",
            div(class = "site-name", where_name),
            div(class = "site-coords", coords)),
        div(class = "site-season",
            div(class = "ss-label", "Stagione"),
            div(class = "ss-value",
                sprintf("%d · trapianto %s", cur_yr, trans_str)))
    )
  })

  # ===== TODAY HERO CARD ==================================================
  # Card grossa in cima: "irriga oggi N mm" / "aspetta pioggia" / "prossima
  # irrig. fra X giorni". Costruita con today_action() (regole, niente LLM).
  today_info <- reactive({
    cur <- current_run()
    om  <- weather_data()
    today_action(cur, om,
                 transplantingDOY  = transplantingDOY(),
                 depletionFraction = input$DepletionFraction)
  })

  output$today_card <- renderUI({
    pt <- selected_point()
    if (is.null(pt)) {
      return(div(class = "today-card", style = "--today-color:#888",
        div(class = "today-icon", "👉"),
        div(class = "today-body",
          div(class = "today-date", "BENVENUTO"),
          div(class = "today-headline",
              "Clicca un punto sulla mappa per scegliere il tuo campo"),
          div(class = "today-detail",
              "Tireremo giu' meteo storico e forecast da Open-Meteo, ",
              "simuleremo la stagione e ti diremo cosa fare oggi."))
      ))
    }

    info <- today_info()
    today <- Sys.Date()
    weekday_it <- c("Domenica","Lunedi","Martedi","Mercoledi",
                    "Giovedi","Venerdi","Sabato")
    date_str <- sprintf("%s %s",
                        weekday_it[as.POSIXlt(today)$wday + 1L],
                        format(today, "%d %B %Y"))

    phase_badge <- if (!is.null(info$phase) && nzchar(info$phase) &&
                       info$phase != "—")
      span(class = "phase-badge",
           sprintf("%s %s", info$phase_icon, info$phase))
      else NULL

    div(class = "today-card",
        style = sprintf("--today-color:%s", info$color),
        div(class = "today-icon", info$icon),
        div(class = "today-body",
            div(class = "today-date", date_str),
            div(class = "today-headline", info$headline),
            div(class = "today-detail", phase_badge, info$detail))
    )
  })

  # ===== Confronto col passato in linguaggio naturale =====================
  output$vs_hist_text <- renderUI({
    cur <- current_run();  h <- historical_runs()
    if (is.null(cur) || is.null(h) || !nrow(cur) || !nrow(h)) return(NULL)

    pct <- ftsw_percentile_today(cur, h, today = Sys.Date())
    if (!is.finite(pct)) return(NULL)

    # pct = 0.20 -> piu' secco dell'80% degli anni; pct = 0.80 -> piu' umido
    descr <- if (pct < 0.25) sprintf("piu' SECCO del %.0f%% degli anni passati",
                                     (1 - pct) * 100)
             else if (pct < 0.50) sprintf("leggermente piu' secco della media (%.0f%%-ile)",
                                          pct * 100)
             else if (pct < 0.75) sprintf("leggermente piu' umido della media (%.0f%%-ile)",
                                          pct * 100)
             else sprintf("piu' UMIDO del %.0f%% degli anni passati", pct * 100)

    div(class = "vs-hist-text",
        strong("Storico vs oggi: "),
        sprintf("a questa data, l'acqua nel terreno e' %s.", descr))
  })

  # ===== Status bar =======================================================
  output$status_bar <- renderUI({
    pt <- selected_point()
    nm <- place_name()
    pt_txt <- if (is.null(pt))
      tags$em("👉 clicca sulla mappa (o usa la ricerca) per scegliere un sito")
    else if (!is.null(nm) && nzchar(nm))
      HTML(sprintf("📍 <strong>%s</strong> <span style='color:#888'>(%.3f°N, %.3f°E)</span>",
                   htmltools::htmlEscape(nm), pt[2], pt[1]))
    else sprintf("📍 %.3f°N  %.3f°E", pt[2], pt[1])

    cur     <- current_run()
    h       <- historical_runs()
    cur_yr  <- as.integer(format(Sys.Date(), "%Y"))
    sim_txt  <- if (is.null(cur)) tags$em("pre-trapianto / nessun dato")
                else sprintf("%d giorni", nrow(cur))
    hist_txt <- if (is.null(h))   tags$em("—")
                else sprintf("%d anni", length(unique(h$year)))

    fc_badge <- NULL
    if (!is.null(cur) && any(cur$is_forecast, na.rm = TRUE)) {
      n_fc <- sum(cur$is_forecast, na.rm = TRUE)
      fc_badge <- span(
        style = paste("background:#ff9800;color:#fff;padding:1px 8px;",
                      "border-radius:10px;font-size:11px;margin-left:8px;",
                      "font-weight:600;"),
        sprintf("🔮 %d giorni forecast", n_fc)
      )
    }

    tagList(
      strong("Sito: "), pt_txt, "   • ",
      strong("Stagione: "), cur_yr, "   • ",
      strong("In stagione: "), sim_txt, "   • ",
      strong("Storico: "), hist_txt,
      fc_badge
    )
  })

  # ===== KPI box ==========================================================
  # 4 carte essenziali. Per yield/brix/irrig usa la MEDIANA dell'ensemble
  # (= proiezione di fine ciclo basata su attuale + analoghi storici) e
  # mostra l'intervallo P10-P90 sotto, vs lo storico assoluto.
  output$kpi_box <- renderUI({
    cur <- current_run()
    h   <- historical_runs()
    ens <- current_run_ensemble()

    # ---- Mediane storico (riferimento "vs storico") ----------------------
    hist_yield <- hist_brix <- hist_irr_mm <- NA_real_
    if (!is.null(h) && nrow(h)) {
      last_h <- h |> group_by(year) |> slice_tail(n = 1L) |> ungroup()
      hist_yield <- median(last_h$fruitFreshWeightAct / 100, na.rm = TRUE)
      if (!is.finite(hist_yield))
        hist_yield <- median(last_h$fruitsStateAct / 100, na.rm = TRUE)
      hist_brix   <- median(last_h$brixAct, na.rm = TRUE)
      tot         <- tapply(h$irrigation, h$year, sum, na.rm = TRUE)
      hist_irr_mm <- median(tot, na.rm = TRUE)
    }

    # ---- Quantili ensemble (P10/P50/P90) ---------------------------------
    ens_endcol_q <- function(col, scale = 1) {
      if (is.null(ens) || !nrow(ens)) return(NULL)
      pp <- tapply(ens[[col]], ens$template_year, function(x) {
        x <- x[is.finite(x)]
        if (length(x)) tail(x, 1L) * scale else NA_real_
      })
      pp <- pp[is.finite(pp)]
      if (length(pp) < 2L) return(NULL)
      unname(quantile(pp, c(0.10, 0.50, 0.90), na.rm = TRUE))
    }
    ens_sumcol_q <- function(col) {
      if (is.null(ens) || !nrow(ens)) return(NULL)
      pp <- tapply(ens[[col]], ens$template_year, sum, na.rm = TRUE)
      pp <- pp[is.finite(pp)]
      if (length(pp) < 2L) return(NULL)
      unname(quantile(pp, c(0.10, 0.50, 0.90), na.rm = TRUE))
    }
    yield_q <- ens_endcol_q("fruitFreshWeightAct", 1/100)
    brix_q  <- ens_endcol_q("brixAct")
    irr_q   <- ens_sumcol_q("irrigation")

    # ---- Helpers UI -------------------------------------------------------
    delta_pct <- function(a, b) {
      if (!is.finite(a) || !is.finite(b) || b == 0) return(NA_real_)
      100 * (a - b) / b
    }
    vs_tag <- function(d) {
      if (!is.finite(d)) return(NULL)
      arrow <- if (d > 0) "▲" else if (d < 0) "▼" else "•"
      col   <- if (d > 0) "#388e3c" else if (d < 0) "#c62828" else "#888"
      span(class = "kpi-vs", style = sprintf("color:%s;", col),
           sprintf("%s %+.0f%% vs storico", arrow, d))
    }
    # NB: i ventagli P10-P90 sono ancora calcolati dietro le quinte (yield_q,
    # brix_q, irr_q) ma NON mostrati sulle KPI card per chiarezza/sintesi.
    # Compaiono ancora nel testo dell'LLM e nel sintetico rule-based.
    kpi <- function(label, value, unit = "", delta = NA,
                    range = NULL, klass = "", value_fmt = "%.1f") {
      div(class = paste("kpi-card", klass),
          div(class = "kpi-label", label),
          div(class = "kpi-value",
              if (is.finite(value)) sprintf(value_fmt, value) else "—"),
          if (nzchar(unit)) div(class = "kpi-unit", unit),
          vs_tag(delta))
    }

    # ---- No simulazione corrente -> mostra solo storico ------------------
    if (is.null(cur) || !nrow(cur)) {
      return(div(class = "kpi-row",
        kpi("🍅 Yield (storico)",  hist_yield,  "t·ha⁻¹"),
        kpi("🍯 Brix (storico)",   hist_brix,   "°"),
        kpi("💧 Irrig. (storico)", hist_irr_mm, "mm"),
        kpi("⏳ Stagione",          NA_real_,    "—")
      ))
    }

    # IMPORTANTE: i KPI di FINE CICLO (yield, brix, irrig totale) sono
    # significativi solo come PROIEZIONE dell'ensemble. Il run deterministico
    # cur si ferma a today+16, quindi il suo tail e' un valore mid-season
    # (Brix=0, yield parziale, irrig parziale). Se l'ensemble non e' ancora
    # pronto, mostriamo "—" invece di numeri sbagliati.
    yield_t <- if (!is.null(yield_q)) yield_q[2] else NA_real_
    brix    <- if (!is.null(brix_q))  brix_q[2]  else NA_real_
    irr_mm  <- if (!is.null(irr_q))   irr_q[2]   else NA_real_

    # Per il KPI "irrig. fatte finora" usiamo il deterministico (e' osservato).
    irr_to_date <- sum(cur$irrigation[as.Date(cur$DATE) <= Sys.Date()],
                       na.rm = TRUE)
    if (!is.finite(irr_to_date)) irr_to_date <- 0

    div(class = "kpi-row",
        kpi("🍅 Yield previsto",   yield_t, "t·ha⁻¹",
            delta = delta_pct(yield_t, hist_yield),
            range = yield_q),
        kpi("🍯 Brix previsto",    brix,    "°",
            delta = delta_pct(brix, hist_brix),
            range = brix_q,
            value_fmt = "%.2f"),
        kpi("💧 Irrig. fine ciclo", irr_mm, "mm",
            delta = delta_pct(irr_mm, hist_irr_mm),
            range = irr_q,
            klass = "irrig-events",
            value_fmt = "%.0f"),
        kpi("💦 Irrig. fatte",     irr_to_date, "mm",
            klass = "irrig-events",
            value_fmt = "%.0f")
    )
  })

  # ===== Forecast strip (horizontal, farmer-friendly) =====================
  output$forecast_strip <- renderUI({
    om <- weather_data()
    if (is.null(om) || !nrow(om)) return(NULL)

    today <- Sys.Date()
    fc <- om[as.Date(om$DATE) >= today, , drop = FALSE]
    if (!nrow(fc)) return(NULL)

    cur <- current_run()
    fc$irr_mm <- 0
    if (!is.null(cur) && nrow(cur)) {
      m <- match(as.Date(fc$DATE), as.Date(cur$DATE))
      irrs <- as.numeric(cur$irrigation[m])
      irrs[is.na(irrs)] <- 0
      fc$irr_mm <- irrs
    }

    cur_yr     <- as.integer(format(today, "%Y"))
    trans_date <- as.Date(sprintf("%d-01-01", cur_yr)) +
                  (as.integer(transplantingDOY()) - 1L)
    end_date   <- trans_date + 150L

    weather_icon <- function(P, Tx, irr_mm) {
      if (is.na(irr_mm)) irr_mm <- 0
      if (irr_mm > 0)    return("\U0001F4A6")    # 💦 sprinkler
      if (is.na(P))  P  <- 0
      if (is.na(Tx)) Tx <- 20
      # Sotto la soglia .RAIN_DAY_MM (default 10mm) NON e' "pioggia vera".
      if (P >= .RAIN_DAY_MM) return("\U0001F327")     # 🌧
      if (Tx < 12)      return("\U0001F325")     # 🌥
      if (Tx > 32)      return("\U0001F525")     # 🔥
      "\u2600\uFE0F"                              # ☀
    }
    advice_for <- function(date, P, irr_mm) {
      if (date < trans_date)
        return(list(text = "\U0001F331 pre-trapianto", cls = "advice-pre"))
      if (date > end_date)
        return(list(text = "\U0001F345 post-ciclo", cls = "advice-pre"))
      if (irr_mm > 0)
        return(list(text = sprintf("\U0001F4A7 %.0f mm", irr_mm),
                    cls = "advice-irrigate"))
      if (!is.na(P) && P >= .RAIN_DAY_MM)
        return(list(text = sprintf("\U0001F327 pioggia %.0f mm", P),
                    cls = "advice-rain"))
      list(text = "\u2713 ok", cls = "advice-ok")
    }

    weekday_it <- function(d) {
      n <- c("Domenica","Lunedì","Martedì","Mercoledì",
             "Giovedì","Venerdì","Sabato")
      n[as.POSIXlt(d)$wday + 1L]
    }

    cards <- lapply(seq_len(nrow(fc)), function(i) {
      d  <- as.Date(fc$DATE[i])
      Tx <- fc$Tx[i]; Tn <- fc$Tn[i]; P <- fc$P[i]
      adv <- advice_for(d, P, fc$irr_mm[i])
      day_class <- "fc-day"
      if (d == today)      day_class <- paste(day_class, "today")
      if (d == trans_date) day_class <- paste(day_class, "transplant")
      if (fc$irr_mm[i] > 0) day_class <- paste(day_class, "irr-day")

      big_mm <- if (fc$irr_mm[i] > 0)
        div(class = "fc-bigmm", sprintf("%.0f mm", fc$irr_mm[i]))
        else NULL

      div(class = day_class,
          div(class = "fc-icon",  HTML(weather_icon(P, Tx, fc$irr_mm[i]))),
          div(class = "fc-day-name", weekday_it(d)),
          div(class = "fc-date",  format(d, "%d/%m/%Y")),
          big_mm,
          div(class = "fc-stats",
              div(sprintf("Tmax %s%.1f °C",
                          ifelse(is.na(Tx), "", ""),
                          ifelse(is.na(Tx), 0, Tx))),
              div(sprintf("Tmin %.1f °C", ifelse(is.na(Tn), 0, Tn))),
              div(sprintf("Pioggia %.1f mm", ifelse(is.na(P), 0, P)))),
          div(class = paste("fc-advice", adv$cls), HTML(adv$text)))
    })

    n_irr_fc  <- sum(fc$irr_mm > 0, na.rm = TRUE)
    irr_mm_fc <- sum(fc$irr_mm,     na.rm = TRUE)

    tagList(
      div(class = "fc-strip-header",
          strong(sprintf("\U0001F5D3 Prossimi %d giorni", nrow(fc))),
          span(class = "fc-strip-sub",
               sprintf("%d intervent%s consigliat%s — %.0f mm totali",
                       n_irr_fc,
                       ifelse(n_irr_fc == 1L, "o", "i"),
                       ifelse(n_irr_fc == 1L, "o", "i"),
                       irr_mm_fc))),
      div(class = "forecast-strip", do.call(tagList, cards))
    )
  })

  # ===== Plot wrapper: empty placeholder OR plotly ========================
  output$mainPlot_wrap <- renderUI({
    cur <- current_run()
    env <- hist_envelope()
    if (is.null(cur) && is.null(env)) {
      pt <- selected_point()
      if (is.null(pt))
        return(div(class = "plot-empty",
                   div(class = "big", "🗺️"),
                   "Clicca un punto sulla mappa per avviare la simulazione."))
      return(div(class = "plot-empty",
                 div(class = "big", "⏳"),
                 "Caricamento meteo / simulazione in corso…"))
    }
    plotlyOutput("mainPlot", height = 640)
  })

  # ===== Main multi-panel plot (SOLO 2 PANNELLI) ==========================
  # Panel 1 (top)    : Canopy % + Yield (t/ha) + Brix * 6 + Fioritura * 80
  #                    Pioggia (>=10mm, blu) + Irrigazione (rosso) come barre y2.
  #                    Tutte le serie scalate per stare comode su 0..150.
  # Panel 2 (bottom) : Acqua nel suolo (FTSW * 100, %) + Water stress
  #                    (riportato sull'asse 0..100 come "stress %"). Tre soglie
  #                    tratteggiate per fase fenologica (vegetativa/riprodutt./ripening).
  #
  # Niente terzo pannello water stress separato: l'utente vede TUTTO l'aspetto
  # idrico (acqua disponibile + stress) sullo stesso grafico, in modo compatto.
  #
  # AFFIDABILITA' DECRESCENTE: dopo today+16 (fine forecast), aggiungiamo una
  # banda grigia che si fa via via piu' opaca per indicare la minore certezza
  # delle proiezioni dell'ensemble (analoghi storici).
  output$mainPlot <- renderPlotly({
    cur     <- current_run()
    env     <- hist_envelope()
    env_ens <- current_run_envelope()
    if (is.null(cur) && is.null(env)) return(NULL)

    has_cur <- !is.null(cur)     && nrow(cur)     > 0
    has_env <- !is.null(env)     && nrow(env)     > 0
    has_ens <- !is.null(env_ens) && nrow(env_ens) > 0

    # ---- X axis range : full season -------------------------------------
    today <- Sys.Date()
    xs <- list()
    if (has_env) xs <- c(xs, list(range(env$DATE,     na.rm = TRUE)))
    if (has_cur) xs <- c(xs, list(range(cur$DATE,     na.rm = TRUE)))
    if (has_ens) xs <- c(xs, list(range(env_ens$DATE, na.rm = TRUE)))
    xs <- c(xs, list(c(today, today + 16L)))
    x_min <- min(do.call(c, xs), na.rm = TRUE)
    x_max <- max(do.call(c, xs), na.rm = TRUE)

    # ---- Reference shapes (today + forecast band) -----------------------
    has_fc <- has_cur && any(cur$is_forecast, na.rm = TRUE)
    fc_start <- today
    fc_end   <- min(today + 16L, x_max)

    base_shapes <- list(
      # Today (solid green)
      list(type = "line", xref = "x", yref = "paper",
           x0 = today, x1 = today, y0 = 0, y1 = 1,
           line = list(color = "#2e7d32", width = 1.6)),
      # Forecast band 0..16 gg (arancione tenue: previsione AFFIDABILE)
      list(type = "rect", xref = "x", yref = "paper",
           x0 = fc_start, x1 = fc_end, y0 = 0, y1 = 1,
           fillcolor = "rgba(255,160,0,0.10)",
           line = list(width = 0), layer = "below"),
      # Forecast end (dashed orange)
      list(type = "line", xref = "x", yref = "paper",
           x0 = fc_end, x1 = fc_end, y0 = 0, y1 = 1,
           line = list(color = "#ef6c00", width = 1.4, dash = "dash"))
    )

    # ---- Banda di INCERTEZZA crescente (dopo fine forecast) -------------
    # Dividiamo l'intervallo (fc_end .. x_max) in 4 segmenti, ognuno piu'
    # opaco del precedente. L'utente vede a colpo d'occhio che le proiezioni
    # via via si allontanano nel tempo e perdono affidabilita'.
    if (x_max > fc_end + 1L) {
      total_days <- as.integer(x_max - fc_end)
      n_seg <- 4L
      seg_len <- max(1L, ceiling(total_days / n_seg))
      for (k in seq_len(n_seg)) {
        s_start <- fc_end + (k - 1L) * seg_len
        s_end   <- min(fc_end + k * seg_len, x_max)
        if (s_end <= s_start) next
        alpha <- 0.05 + 0.05 * k     # 0.10 -> 0.25 progressivo
        base_shapes <- c(base_shapes, list(
          list(type = "rect", xref = "x", yref = "paper",
               x0 = s_start, x1 = s_end, y0 = 0, y1 = 1,
               fillcolor = sprintf("rgba(120,120,120,%.3f)", alpha),
               line = list(width = 0), layer = "below")
        ))
      }
    }

    # Marker fioritura piena (giorno con max floweringRateAct)
    flo_peak_date <- NULL
    if (has_cur && "floweringRateAct" %in% names(cur)) {
      idx <- which.max(cur$floweringRateAct)
      if (length(idx) && cur$floweringRateAct[idx] > 0)
        flo_peak_date <- as.Date(cur$DATE[idx])
    }
    if (!is.null(flo_peak_date)) {
      base_shapes <- c(base_shapes, list(
        list(type = "line", xref = "x", yref = "paper",
             x0 = flo_peak_date, x1 = flo_peak_date, y0 = 0, y1 = 1,
             line = list(color = "#f9a825", width = 1.6, dash = "dot"))
      ))
    }

    # Solo ribbon (P10-P90) per lo storico — niente mediana tratteggiata,
    # mantiene l'occhio sull'andamento corrente.
    add_ribbon <- function(p, var, color_rgba, color_line, name_pref,
                           yaxis = "y", scale = 1) {
      if (!has_env) return(p)
      lo <- paste0(var, "_p10"); hi <- paste0(var, "_p90")
      if (!all(c(lo, hi) %in% names(env))) return(p)
      p |>
        add_trace(x = env$DATE, y = env[[hi]] * scale,
                  type = "scatter", mode = "lines",
                  line = list(width = 0), showlegend = FALSE,
                  hoverinfo = "skip", yaxis = yaxis, inherit = FALSE) |>
        add_trace(x = env$DATE, y = env[[lo]] * scale,
                  type = "scatter", mode = "lines",
                  line = list(width = 0), fill = "tonexty",
                  fillcolor = color_rgba,
                  name = paste0(name_pref, " storico P10–P90"),
                  hovertemplate = paste0(name_pref, " hist<extra></extra>"),
                  yaxis = yaxis, inherit = FALSE)
    }

    # Forecast (analoghi storici) — SOLO mediana tratteggiata, niente ribbon.
    # Il ribbon resta riservato allo storico (per non sovraccaricare il plot).
    add_ens_fan <- function(p, var, color_rgba, color_line, name_pref,
                            yaxis = "y", scale = 1) {
      if (!has_ens) return(p)
      md <- paste0(var, "_p50")
      if (!(md %in% names(env_ens))) return(p)
      ef <- env_ens[env_ens$DATE >= today + 16L, , drop = FALSE]
      if (!nrow(ef)) return(p)
      p |>
        add_trace(x = ef$DATE, y = ef[[md]] * scale,
                  type = "scatter", mode = "lines",
                  line = list(color = color_line, width = 2.2, dash = "dash"),
                  name = paste0(name_pref, " proiezione (mediana analoghi)"),
                  hovertemplate = paste0(name_pref,
                                         " proiezione: %{y:.1f}<extra></extra>"),
                  yaxis = yaxis, inherit = FALSE)
    }

    # ============== Panel 1: Canopy + Yield + Brix + Fioritura + bars ====
    # SCALE per metterli tutti su asse 0..150 senza che si schiaccino:
    #   Canopy %         : 0..100  (gia' su scala)
    #   Yield t/ha       : 0..120  (gia' su scala)
    #   Brix °           : 0..15  -> *6  -> 0..90 visibile
    #   Fioritura (0..1) : 0..1   -> *80 -> 0..80 visibile (campana)
    BRIX_SCALE <- 6
    FLO_SCALE  <- 80
    p1 <- plot_ly() |>
      add_ribbon("fIntAct", "rgba(76,175,80,0.18)", "#558b2f",
                 "Canopy %", scale = 100) |>
      # Fan ensemble Yield (viola chiaro) — solo proiezione futura
      add_ens_fan("fruitFreshWeightAct", "rgba(106,27,154,0.18)", "#6a1b9a",
                  "Yield (t/ha)", scale = 1/100) |>
      add_ens_fan("brixAct", "rgba(239,108,0,0.16)", "#c62828",
                  "Brix ×6", scale = BRIX_SCALE)

    # Irrigazioni STORICHE (mediana per DOY) come barre tenue, asse y2.
    # Cosi' l'agricoltore vede dove e quanto ha irrigato il modello negli
    # anni passati, e puo' confrontare col run corrente.
    if (has_env && "irrigation_p50" %in% names(env)) {
      p1 <- p1 |>
        add_trace(x = env$DATE, y = env$irrigation_p50,
                  type = "bar", name = "Irrig. mediana storica (mm)",
                  marker = list(color = "rgba(229,57,53,0.30)",
                                line = list(width = 0)),
                  yaxis = "y2", opacity = 0.85,
                  hovertemplate = "Irrig. storica mediana: %{y:.0f} mm<extra></extra>",
                  inherit = FALSE)
    }
    if (has_cur) {
      # Ordine traces studiato per VISIBILITA':
      #  1) Canopy con fill TENUE (alpha 0.18) sotto tutto.
      #  2) Yield, brix, fioritura come linee SOPRA.
      #  3) Pioggia (>=10mm) e irrigazione come barre su y2.
      p1 <- p1 |>
        # Canopy (line + fill tenue, sotto le linee)
        add_trace(x = cur$DATE, y = cur$fIntAct * 100,
                  type = "scatter", mode = "lines",
                  fill = "tozeroy", fillcolor = "rgba(76,175,80,0.18)",
                  name = "🌿 Canopy %",
                  line = list(color = "#558b2f", width = 1.8),
                  hovertemplate = "Canopy: %{y:.1f}%<extra></extra>",
                  inherit = FALSE)

      # Fioritura — sempre disegnata. Scaliamo (0..1) -> (0..FLO_SCALE) per
      # essere visibile sul grafico canopy/yield. La forma a campana resta.
      if ("floweringRateAct" %in% names(cur) ||
          "floweringStateAct" %in% names(cur)) {
        # Preferiamo floweringRateAct (campana) se c'e'. Altrimenti usiamo
        # un proxy come derivata discreta di floweringStateAct.
        flo_y <- if ("floweringRateAct" %in% names(cur)) {
          as.numeric(cur$floweringRateAct)
        } else {
          fs <- as.numeric(cur$floweringStateAct)
          c(0, diff(fs))
        }
        flo_max_loc <- max(flo_y, na.rm = TRUE)
        if (!is.finite(flo_max_loc) || flo_max_loc <= 0) flo_max_loc <- 1
        flo_norm <- pmin(flo_y / flo_max_loc, 1)        # 0..1
        flo_disp <- flo_norm * FLO_SCALE                # 0..80
        p1 <- p1 |>
          add_trace(x = cur$DATE, y = flo_disp,
                    type = "scatter", mode = "lines",
                    name = "🌸 Fioritura (campana)",
                    line = list(color = "#e91e63", width = 3),
                    customdata = flo_norm,
                    hovertemplate = "Fioritura: %{customdata:.2f} (norm.)<extra></extra>",
                    inherit = FALSE)
      }
      p1 <- p1 |>
        # Yield (viola)
        add_trace(x = cur$DATE, y = cur$fruitFreshWeightAct / 100,
                  type = "scatter", mode = "lines",
                  name = "🍅 Yield (t/ha)",
                  line = list(color = "#6a1b9a", width = 2.8),
                  hovertemplate = "Yield: %{y:.1f} t/ha<extra></extra>",
                  inherit = FALSE) |>
        # Brix (rosso) — *6 per essere ben visibile (Brix da 0 a ~15)
        add_trace(x = cur$DATE, y = cur$brixAct * BRIX_SCALE,
                  type = "scatter", mode = "lines+markers",
                  name = "🍯 Brix ×6 (Brix °)",
                  line   = list(color = "#c62828", width = 2.6),
                  marker = list(color = "#c62828", size = 4),
                  customdata = cur$brixAct,
                  hovertemplate = "Brix: %{customdata:.2f}°<extra></extra>",
                  inherit = FALSE) |>
        # Pioggia EFFICACE (>= 10 mm) — bars on y2.
        add_trace(x = cur$DATE,
                  y = ifelse(is.finite(cur$p) & cur$p >= .RAIN_DAY_MM,
                             cur$p, 0),
                  type = "bar",
                  name = sprintf("🌧 Pioggia >=%.0f mm", .RAIN_DAY_MM),
                  marker = list(color = "rgba(25,118,210,0.75)"),
                  hovertemplate = "Pioggia: %{y:.0f} mm<extra></extra>",
                  yaxis = "y2", inherit = FALSE) |>
        # Irrigazione consigliata dal modello (rosso, asse y2). Disegnata
        # con un bordo cosi' si vede chiaramente vs. pioggia blu.
        add_trace(x = cur$DATE, y = cur$irrigation,
                  type = "bar", name = "💧 Irrig. consigliata",
                  marker = list(color = "rgba(229,57,53,0.95)",
                                line = list(color = "#b71c1c", width = 0.5)),
                  yaxis = "y2", inherit = FALSE)

      # Sovrapponiamo le OVERRIDE dell'agricoltore (skip + applied) come
      # marker addizionali su y2. Cosi' a colpo d'occhio:
      #   barra rossa  = consiglio del modello (puo' essere anche saltato)
      #   X grigia     = consigliata MA saltata (sopra alla barra)
      #   barra verde  = irrigazione FATTA dall'agricoltore (in piu')
      ovr <- irr_overrides()
      if (length(ovr)) {
        skip_keys <- names(ovr)[vapply(ovr,
          function(x) identical(x$action, "skip"), logical(1))]
        appl_keys <- names(ovr)[vapply(ovr,
          function(x) identical(x$action, "applied"), logical(1))]

        # X sui giorni saltati (asse y2, alla stessa altezza della barra
        # consigliata, simbolo "x" grigio scuro)
        if (length(skip_keys)) {
          skip_dates <- as.Date(skip_keys)
          skip_y <- vapply(skip_dates, function(d) {
            ix <- which(as.Date(cur$DATE) == d)
            if (length(ix)) as.numeric(cur$irrigation[ix[1]]) else 0
          }, numeric(1))
          p1 <- p1 |>
            add_trace(x = skip_dates, y = skip_y,
                      type = "scatter", mode = "markers",
                      name = "🚫 Saltata",
                      marker = list(symbol = "x-thin", size = 14,
                                    color = "#424242",
                                    line = list(color = "#212121",
                                                width = 2.5)),
                      yaxis = "y2", inherit = FALSE,
                      hovertemplate = "Saltata: %{y:.0f} mm<extra></extra>")
        }

        # Barre verdi per le irrigazioni FATTE dall'agricoltore
        if (length(appl_keys)) {
          appl_dates <- as.Date(appl_keys)
          appl_mm <- vapply(appl_keys, function(k)
                            as.numeric(ovr[[k]]$mm %||% 0),
                            numeric(1))
          p1 <- p1 |>
            add_trace(x = appl_dates, y = appl_mm,
                      type = "bar", name = "💚 Fatta da te",
                      marker = list(color = "rgba(46,125,50,0.92)",
                                    line = list(color = "#1b5e20",
                                                width = 0.5)),
                      yaxis = "y2", inherit = FALSE,
                      hovertemplate = "Tu hai irrigato: %{y:.0f} mm<extra></extra>")
        }
      }
    }

    p1 <- p1 |> layout(
      yaxis  = list(title = paste("Canopy % | Yield t/ha | Brix ×6 | Fioritura ×80",
                                  "(scale per visibilita')"),
                    range = c(0, 150), zeroline = TRUE),
      yaxis2 = list(title = "Pioggia / Irrigazione (mm/giorno)",
                    overlaying = "y",
                    side = "right", showgrid = FALSE,
                    range = c(0, 50)),
      shapes = base_shapes,
      barmode = "group"
    )

    # ============== Panel 2: ACQUA NEL SUOLO + WATER STRESS ==============
    # Asse 0..100 (% di acqua disponibile nel suolo). Niente "FTSW".
    # Sovrapposto: water stress come "stress %" (linea rossa, 0=ok, 100=stress).
    # Tre soglie per fase fenologica disegnate solo durante la fase relativa.
    p2 <- plot_ly() |>
      add_ribbon("ftsw", "rgba(63,81,181,0.18)", "#3949ab",
                 "Acqua nel suolo storico", scale = 100) |>
      add_ens_fan("ftsw", "rgba(0,131,143,0.18)", "#00838f",
                  "Acqua nel suolo", scale = 100)

    if (has_cur && "ftsw" %in% names(cur)) {
      p2 <- p2 |>
        add_trace(x = cur$DATE, y = cur$ftsw * 100,
                  type = "scatter", mode = "lines",
                  name = "💧 Acqua nel suolo (%)",
                  line = list(color = "#1565c0", width = 2.6),
                  fill = "tozeroy",
                  fillcolor = "rgba(33,150,243,0.18)",
                  customdata = cur$ftsw,
                  hovertemplate = "Acqua nel suolo: %{y:.0f}%<extra></extra>",
                  inherit = FALSE)
    }

    # Stress idrico (1 - waterStress per leggibilita': 0=ok, 100=stress max)
    if (has_cur && "waterStress" %in% names(cur)) {
      stress_pct <- (1 - cur$waterStress) * 100
      p2 <- p2 |>
        add_trace(x = cur$DATE, y = stress_pct,
                  type = "scatter", mode = "lines",
                  name = "🥵 Stress idrico (%)",
                  line = list(color = "#c62828", width = 2.2, dash = "dot"),
                  hovertemplate = "Stress idrico: %{y:.0f}%<extra></extra>",
                  inherit = FALSE)
    }

    # Marker sul valore di OGGI (acqua nel suolo)
    today_marker_shape <- list()
    today_marker_ann   <- list()
    if (has_cur) {
      it <- which(as.Date(cur$DATE) == today)
      if (length(it)) {
        wpct_today <- as.numeric(cur$ftsw[it]) * 100
        today_marker_shape <- list(
          list(type = "circle", xref = "x", yref = "y",
               x0 = today - 1L, x1 = today + 1L,
               y0 = wpct_today - 2.5, y1 = wpct_today + 2.5,
               fillcolor = "#d32f2f",
               line = list(color = "#d32f2f", width = 1))
        )
        today_marker_ann <- list(list(
          x = today, y = wpct_today + 8, xref = "x", yref = "y",
          text = sprintf("oggi: %.0f%%", wpct_today),
          showarrow = FALSE,
          font = list(size = 11, color = "#d32f2f"),
          bgcolor = "rgba(255,255,255,0.85)"
        ))
      }
    }

    # ---- Soglie water stress per fase, riportate su scala 0..100 --------
    # (mostriamo come "soglia stress %" = (1 - ws_threshold) * 100 cosi' chi
    # legge il grafico vede dove il modello inizia a irrigare).
    p2_shapes <- c(base_shapes, today_marker_shape)
    p2_anns   <- today_marker_ann
    phase_threshold_segment <- function(phase_code, ws_threshold,
                                        color, label) {
      if (!has_cur) return(NULL)
      if (!("phenoCode" %in% names(cur))) return(NULL)
      idx <- which(as.integer(cur$phenoCode) == phase_code)
      if (!length(idx)) return(NULL)
      x0 <- as.Date(min(cur$DATE[idx]))
      x1 <- as.Date(max(cur$DATE[idx]))
      stress_pct_threshold <- (1 - ws_threshold) * 100
      shp <- list(
        type = "line", xref = "x", yref = "y",
        x0 = x0, x1 = x1,
        y0 = stress_pct_threshold, y1 = stress_pct_threshold,
        line = list(color = color, width = 2.2, dash = "dash")
      )
      ann <- list(
        x = x0 + as.integer(x1 - x0) / 2,
        y = stress_pct_threshold + 5,
        xref = "x", yref = "y",
        text = sprintf("%s · soglia stress %.0f%%", label,
                       stress_pct_threshold),
        showarrow = FALSE,
        font = list(size = 10, color = color),
        bgcolor = "rgba(255,255,255,0.85)"
      )
      list(shape = shp, ann = ann)
    }

    seg_v <- phase_threshold_segment(2L, as.numeric(input$ws_veg),
                                     "#43a047", "🌱 Vegetativa")
    seg_r <- phase_threshold_segment(3L, as.numeric(input$ws_rep),
                                     "#1e88e5", "🌸 Riproduttiva")
    seg_p <- phase_threshold_segment(4L, as.numeric(input$ws_rip),
                                     "#fb8c00", "🍅 Maturazione")
    for (s in list(seg_v, seg_r, seg_p)) {
      if (!is.null(s)) {
        p2_shapes <- c(p2_shapes, list(s$shape))
        p2_anns   <- c(p2_anns,   list(s$ann))
      }
    }

    p2 <- p2 |> layout(
      yaxis  = list(title = "Acqua nel suolo / Stress idrico (%)",
                    range = c(0, 105), zeroline = TRUE),
      shapes = p2_shapes,
      annotations = p2_anns
    )

    # ============== Stitch panels (SOLO 2) ===============================
    sp <- subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE,
                  heights = c(0.55, 0.45)) |>
      layout(legend = list(orientation = "h", y = -0.10,
                           font = list(size = 10)),
             margin = list(l = 55, r = 55, t = 28, b = 50),
             hovermode = "x unified",
             # Asse X: date sempre visibili, tick mensili, formato giorno+mese
             xaxis = list(range = c(x_min, x_max),
                          title = "",
                          type = "date",
                          tickformat = "%d %b",
                          dtick = "M1",
                          tickfont = list(size = 11, color = "#222"),
                          showgrid = TRUE,
                          gridcolor = "#eef0f2",
                          ticks = "outside",
                          ticklen = 5))

    fan_lbl <- if (has_ens) " | inizio ventaglio" else ""
    top_anns <- list(
      list(x = today, y = 1.04, xref = "x", yref = "paper",
           text = "oggi", showarrow = FALSE,
           font = list(size = 10, color = "#2e7d32"), xanchor = "center"),
      list(x = fc_end, y = 1.04, xref = "x", yref = "paper",
           text = paste0("fine forecast", fan_lbl), showarrow = FALSE,
           font = list(size = 10, color = "#ef6c00"), xanchor = "center")
    )
    if (!is.null(flo_peak_date)) {
      top_anns <- c(top_anns, list(list(
        x = flo_peak_date, y = 1.04, xref = "x", yref = "paper",
        text = "🌸 fioritura piena", showarrow = FALSE,
        font = list(size = 10, color = "#f9a825"), xanchor = "center")))
    }
    sp <- sp |> layout(annotations = top_anns)

    sp |> config(displaylogo = FALSE,
                 modeBarButtonsToRemove = c("select2d", "lasso2d"))
  })

  # ===== Feedback agricoltore: tabella consigli vs fatti ==================
  # Mostra SOLO le righe (consigliate, saltate, applicate). Il titolo e il
  # form "aggiungi mia irrigazione" stanno in ui.R come UI STATICA, cosi' la
  # date/mm non si resettano ogni volta che le override cambiano.
  output$irrigation_feedback_rows <- renderUI({
    cur <- current_run()
    if (is.null(cur) || !nrow(cur)) {
      return(div(class = "irrf-row empty",
                 em("In attesa dei dati di simulazione...")))
    }

    today <- Sys.Date()
    ovr   <- irr_overrides()

    # Date consigliate dal modello (in ordine cronologico)
    rec_idx   <- which(cur$irrigation > 0)
    rec_dates <- as.Date(cur$DATE[rec_idx])
    rec_mm    <- as.numeric(cur$irrigation[rec_idx])

    # Costruisci righe: prima "consigliate", poi "applicate manualmente"
    rows <- list()

    # --- Consigliate ---------------------------------------------------
    for (i in seq_along(rec_dates)) {
      d   <- rec_dates[i]
      key <- format(d, "%Y-%m-%d")
      mm  <- rec_mm[i]
      is_skipped <- !is.null(ovr[[key]]) && identical(ovr[[key]]$action, "skip")
      is_past    <- d < today
      cls_extra  <- if (is_skipped) "skipped"
                    else if (is_past) "suggested past"
                    else "suggested"
      # Etichetta temporale
      tag_txt <- if (is_skipped) "saltata"
                 else if (is_past) sprintf("consigliata %d gg fa",
                                           as.integer(today - d))
                 else if (d == today) "consigliata OGGI"
                 else sprintf("consigliata fra %d gg",
                              as.integer(d - today))
      # Bottone azione (skip <-> undo)
      if (is_skipped) {
        btn <- tags$button(class = "irrf-act btn-undo",
                           `data-act` = "undo-skip",
                           `data-date` = key,
                           "↩ ripristina")
      } else {
        btn <- tags$button(class = "irrf-act btn-skip",
                           `data-act` = "skip",
                           `data-date` = key,
                           "✕ salto")
      }
      rows <- c(rows, list(
        div(class = paste("irrf-row", cls_extra),
            div(class = "irrf-date", format(d, "%a %d %b")),
            div(class = "irrf-mm",   sprintf("%.0f mm", mm)),
            div(class = "irrf-tag",  tag_txt),
            div(class = "irrf-actions", btn))
      ))
    }

    # --- Applicate manualmente ----------------------------------------
    applied_keys <- names(ovr)[vapply(ovr,
                                      function(x) identical(x$action, "applied"),
                                      logical(1))]
    if (length(applied_keys)) {
      applied_keys <- applied_keys[order(as.Date(applied_keys))]
      for (key in applied_keys) {
        d  <- as.Date(key)
        mm <- as.numeric(ovr[[key]]$mm)
        rows <- c(rows, list(
          div(class = "irrf-row applied",
              div(class = "irrf-date", format(d, "%a %d %b")),
              div(class = "irrf-mm",   sprintf("%.0f mm", mm)),
              div(class = "irrf-tag",  "fatta dall'agricoltore"),
              div(class = "irrf-actions",
                  tags$button(class = "irrf-act btn-remove",
                              `data-act` = "remove-applied",
                              `data-date` = key,
                              "🗑")))
        ))
      }
    }

    # Riga vuota se non c'e' niente da mostrare
    if (!length(rows)) {
      rows <- list(div(class = "irrf-row empty",
                       em("Nessuna irrigazione consigliata in questa stagione.")))
    }

    do.call(tagList, rows)
  })

  # ===== LLM interpretation ===============================================
  llm_text <- reactiveVal(NULL)

  # Costruisce il summary text. Estratto in funzione cosi' lo usano sia il
  # bottone "Spiega" sia l'auto-fire.
  build_llm_summary <- function() {
    cur <- current_run()
    h   <- historical_runs()
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    today  <- Sys.Date()

    hist_summary <- ""
    if (!is.null(h) && nrow(h)) {
      last_h <- h |> group_by(year) |> slice_tail(n = 1L) |> ungroup()
      hist_y <- last_h$fruitFreshWeightAct / 100
      hist_b <- last_h$brixAct
      tot <- tapply(h$irrigation, h$year, sum, na.rm = TRUE)
      cnt <- tapply(h$irrigation > 0, h$year, sum, na.rm = TRUE)
      pct_today <- ftsw_percentile_today(cur, h, today)
      pct_line <- if (is.finite(pct_today))
        sprintf("- FTSW oggi vs storico stesso DOY: percentile %.0f%% (0=piu' secco di tutti, 100=piu' umido).\n",
                pct_today * 100) else ""
      hist_summary <- paste0(sprintf(
        "- Storico (%d anni): yield mediano %.1f t/ha (P10=%.1f, P90=%.1f); brix %.2f; irrig. mediana %.0f mm in %.0f interventi.\n",
        length(unique(h$year)),
        median(hist_y, na.rm = TRUE),
        quantile(hist_y, .10, na.rm = TRUE),
        quantile(hist_y, .90, na.rm = TRUE),
        median(hist_b, na.rm = TRUE),
        median(tot, na.rm = TRUE),
        median(cnt, na.rm = TRUE)
      ), pct_line)
    }

    pt <- selected_point()
    pt_lat <- if (!is.null(pt)) pt[2] else NA
    pt_lon <- if (!is.null(pt)) pt[1] else NA

    if (is.null(cur) || !nrow(cur)) {
      return(sprintf(
        "Stagione %d non ancora avviata (pre-trapianto o senza dati).
- Sito: %.3f deg N, %.3f deg E
- Trapianto: DOY %d
- Strategia: veg WS=%.2f/turn=%dd, repr WS=%.2f/turn=%dd, ripen WS=%.2f/turn=%dd
%s
Compito: 4-5 frasi su cosa aspettarsi alla luce dello storico e della strategia.",
        cur_yr, pt_lat, pt_lon,
        transplantingDOY(),
        input$ws_veg, as.integer(input$turn_veg),
        input$ws_rep, as.integer(input$turn_rep),
        input$ws_rip, as.integer(input$turn_rip),
        hist_summary))
    }

    yield_t <- tail(cur$fruitFreshWeightAct, 1L) / 100
    if (!is.finite(yield_t)) yield_t <- tail(cur$fruitsStateAct, 1L) / 100
    n_irr  <- sum(cur$irrigation > 0, na.rm = TRUE)
    irr_mm <- sum(cur$irrigation, na.rm = TRUE)
    n_fc   <- sum(cur$is_forecast, na.rm = TRUE)

    # FTSW e azione di oggi
    info <- today_info()
    today_line <- sprintf("- AZIONE OGGI consigliata dal modello: %s. %s\n",
                          info$headline, info$detail)
    fc_line <- if (n_fc > 0L) sprintf(
      "- Ultimi %d giorni del run sono FORECAST (Open-Meteo +16d).\n",
      n_fc) else ""

    # Riassunto meteo prossimi 3 gg
    om_now <- weather_data()
    w3_line <- ""
    if (!is.null(om_now)) w3_line <- sprintf("- METEO 3gg: %s\n",
                                             weather_3day_summary(om_now))

    # Feedback agricoltore: irrigazioni saltate / aggiunte (override)
    fb_line <- ""
    ovr_now <- irr_overrides()
    if (length(ovr_now)) {
      sk <- vapply(ovr_now,
                   function(x) identical(x$action, "skip"),
                   logical(1))
      ap <- vapply(ovr_now,
                   function(x) identical(x$action, "applied"),
                   logical(1))
      sk_n  <- sum(sk)
      ap_n  <- sum(ap)
      ap_mm <- if (any(ap)) sum(vapply(ovr_now[ap],
                                       function(x) as.numeric(x$mm %||% 0),
                                       numeric(1)), na.rm = TRUE) else 0
      fb_line <- sprintf(
        "- FEEDBACK AGRICOLTORE: %d consigli SALTATI, %d irrigazioni aggiunte (%.0f mm tot).\n",
        sk_n, ap_n, ap_mm)
    }

    # Quantili ensemble fine ciclo
    ens_now <- current_run_ensemble()
    ens_line <- ""
    if (!is.null(ens_now) && nrow(ens_now)) {
      yq <- tapply(ens_now$fruitFreshWeightAct, ens_now$template_year,
                   function(x) {
                     x <- x[is.finite(x)]
                     if (length(x)) tail(x, 1L) / 100 else NA_real_
                   })
      yq <- yq[is.finite(yq)]
      if (length(yq) >= 2L)
        ens_line <- sprintf(
          "- Ensemble (analoghi storici N=%d): yield previsto P10/P50/P90 = %.1f / %.1f / %.1f t/ha.\n",
          length(yq),
          quantile(yq, .10, na.rm = TRUE),
          quantile(yq, .50, na.rm = TRUE),
          quantile(yq, .90, na.rm = TRUE))
    }

    sprintf(
      "Risultati CUMBA — pomodoro da industria, stagione %d, data %s.
- Sito: %.3f deg N, %.3f deg E
- Trapianto: DOY %d   Ciclo: %d gradi-giorno
- Strategia irrigua: veg WS=%.2f/turn=%dd, repr WS=%.2f/turn=%dd, ripen WS=%.2f/turn=%dd
- Run a oggi: yield previsto %.1f t/ha, brix %.2f deg, %d interventi per %.0f mm tot, WS min %.2f
%s%s%s%s%s%s
Compito: 4-5 frasi corte. Cosa fare nei prossimi giorni? Come va vs storico?
Una sola modifica concreta agli slider se serve. Se l'agricoltore ha
saltato/aggiunto irrigazioni, commenta brevemente la sua scelta.",
      cur_yr, format(today, "%d %B %Y"),
      pt_lat, pt_lon,
      transplantingDOY(), as.integer(input$CycleLength),
      input$ws_veg, as.integer(input$turn_veg),
      input$ws_rep, as.integer(input$turn_rep),
      input$ws_rip, as.integer(input$turn_rip),
      yield_t, tail(cur$brixAct, 1L), n_irr, irr_mm,
      min(cur$waterStress, na.rm = TRUE),
      today_line, w3_line, fb_line, ens_line, hist_summary, fc_line)
  }

  # Stato dell'ultima chiamata LLM:
  # status: "idle"|"loading"|"ok"|"fallback"  + note diagnostica + diag (testo
  # tecnico opzionale con HTTP/model attempts).
  llm_state <- reactiveVal(list(status = "idle", note = NULL, diag = NULL))

  run_llm <- function() {
    summary_text <- build_llm_summary()
    llm_state(list(status = "loading", note = NULL, diag = NULL))
    withProgress(message = "🤖 CUMBA sta pensando...", value = 0.4, {
      txt <- tryCatch(interpret_with_claude(summary_text, language = input$language),
                      error = function(e) {
                        structure(sprintf("Errore di rete: %s", conditionMessage(e)),
                                  class = c("cumba_llm_err", "character"))
                      })
      setProgress(1)
    })

    # Determina se la risposta e' un errore. Il nostro interpret_with_llm
    # marca i fallimenti con classe "cumba_llm_err"; come safety net usa
    # anche heuristic sui prefissi diagnostici.
    is_err <- inherits(txt, "cumba_llm_err") ||
              is.null(txt) || !nzchar(txt) ||
              grepl("^\\[KEY_MISSING\\]", txt) ||
              grepl("^\\[ALL_RATE_LIMITED\\]", txt) ||
              grepl("^\\[LLM_HTTP_", txt) ||
              grepl("^Errore LLM", txt)

    if (is_err) {
      # ---- 1) Fallback rule-based "amico" ---------------------------
      fb <- synth_message(today_info(), weather_data(), current_run(),
                          current_run_ensemble())
      if (!nzchar(fb)) fb <- paste("Aspetto che tu scelga un sito sulla mappa,",
                                   "poi ti racconto come va il campo.")
      llm_text(fb)

      # ---- 2) Diagnostica utente -------------------------------------
      # Estrae il motivo dal testo grezzo dell'errore.
      raw <- as.character(txt)
      note <- if (grepl("\\[KEY_MISSING\\]", raw))
                paste("Chiave LLM non impostata. Ti sto rispondendo con il",
                      "modello (regole), che e' affidabile ma meno",
                      "discorsivo. Per attivare l'LLM (gratis):",
                      "openrouter.ai > Keys > Create Key, poi imposta la",
                      "variabile d'ambiente OPENROUTER_API_KEY.")
              else if (grepl("\\[ALL_RATE_LIMITED\\]", raw))
                paste("I modelli free di OpenRouter sono saturi al momento.",
                      "Sto usando il fallback rule-based. Riprova fra 1-2",
                      "minuti col bottone Riprova.")
              else if (grepl("\\[LLM_HTTP_", raw))
                paste("L'LLM ha risposto con un errore HTTP. Sto usando",
                      "il fallback rule-based. Apri il dettaglio per vedere",
                      "cosa e' successo.")
              else
                paste("L'LLM non ha risposto. Sto usando il fallback",
                      "rule-based — vedi il dettaglio per i tentativi.")

      diag <- if (grepl("Tentativi:", raw)) raw else NULL
      llm_state(list(status = "fallback", note = note, diag = diag))
    } else {
      llm_text(as.character(txt))
      llm_state(list(status = "ok", note = NULL, diag = NULL))
    }
  }

  observeEvent(input$interpretBtn, { run_llm() })

  # Auto-fire dopo ogni run (debounced) se l'utente l'ha attivato
  current_run_d <- reactive({ current_run() }) |> debounce(1500)
  observeEvent(current_run_d(), {
    if (isTRUE(input$autoLLM) && !is.null(current_run_d())) run_llm()
  }, ignoreNULL = TRUE)

  output$interpretation_text <- renderUI({
    txt <- llm_text()
    if (is.null(txt))
      return(em(paste("Scegli un campo sulla mappa, e ti dico subito",
                      "come va e cosa fare nei prossimi giorni.")))
    HTML(gsub("\n", "<br/>", htmltools::htmlEscape(txt)))
  })

  output$llm_status <- renderUI({
    st <- llm_state()
    if (is.null(st) || st$status == "idle" || st$status == "ok") return(NULL)
    if (st$status == "loading") {
      return(div(class = "llm-status",
                 "🤖 sto pensando..."))
    }
    if (st$status == "fallback") {
      note_html <- if (!is.null(st$note))
        div(style = "color:#bf360c;",
            HTML(paste0("⚠ ", htmltools::htmlEscape(st$note))))
        else NULL
      diag_html <- if (!is.null(st$diag) && nzchar(st$diag))
        tags$details(style = "margin-top:6px;",
                     tags$summary("Mostra dettaglio tecnico",
                                  style = "cursor:pointer; font-size:11px; color:#6d4c41;"),
                     tags$pre(st$diag,
                              style = paste("font-size:10.5px; background:#fff;",
                                            "padding:6px 8px; border-radius:4px;",
                                            "border:1px solid #eee; margin-top:4px;",
                                            "white-space:pre-wrap;")))
        else NULL
      return(div(class = "llm-status", note_html, diag_html))
    }
    NULL
  })

  # Pill colorata accanto al titolo "🤖 CUMBA ti spiega"
  output$llm_pill <- renderUI({
    st <- llm_state()
    if (is.null(st) || st$status == "idle") return(NULL)
    if (st$status == "loading")
      return(span(class = "llm-pill fb", "..."))
    if (st$status == "ok")
      return(span(class = "llm-pill ok", "LLM"))
    if (st$status == "fallback")
      return(span(class = "llm-pill fb",
                  title = if (!is.null(st$note)) st$note else "",
                  "REGOLE"))
    NULL
  })
}
