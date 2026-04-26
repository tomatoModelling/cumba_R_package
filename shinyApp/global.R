# shinyApp/global.R --------------------------------------------------------
# Loaded once at app start.

suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(leaflet.extras)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(httr2)
  library(jsonlite)
  library(lubridate)
  library(shinycssloaders)
})

# --- CUMBA loading --------------------------------------------------------
# Always prefer the working source when running from the package directory
# (so phase-specific waterStressLevel/minimumTurn changes are picked up
# without re-installing).
.pkg_root <- normalizePath("..", mustWork = FALSE)
.in_pkg <- file.exists(file.path(.pkg_root, "DESCRIPTION")) &&
           file.exists(file.path(.pkg_root, "R", "Main.R"))

if (.in_pkg && requireNamespace("devtools", quietly = TRUE)) {
  message("Loading cumba from source via devtools::load_all('..')")
  suppressMessages(devtools::load_all(.pkg_root, quiet = TRUE))
} else {
  loaded <- tryCatch(
    { suppressMessages(library(cumba)); TRUE },
    error = function(e) FALSE
  )
  if (!loaded)
    stop("Install the cumba package or run from a dev session with devtools.")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Costanti agronomiche -------------------------------------------------
# Soglia oltre cui un giorno e' considerato "piovoso" agronomicamente:
# sotto ~10 mm la pioggia viene perlopiu' intercettata dal canopy o
# evapora prima di entrare nel suolo radicale, quindi NON sostituisce
# l'irrigazione. Cambia qui per modificare il comportamento di:
#   - Pannello 1 (barre pioggia mostrate solo se >= soglia)
#   - today_action() (rain_next3 conta solo i giorni >= soglia)
#   - forecast strip (advice "pioggia" solo se >= soglia)
#   - synth_message() / weather_3day_summary() etichette
.RAIN_DAY_MM <- 10

# --- Helpers --------------------------------------------------------------
fill_with_avg <- function(x) {
  na <- which(is.na(x))
  if (!length(na)) return(x)
  for (i in na) {
    prev_val <- if (i > 1)         tail(x[seq_len(i - 1L)], 1L) else NA_real_
    next_val <- if (i < length(x)) head(x[(i + 1L):length(x)], 1L) else NA_real_
    x[i] <- if (!is.na(prev_val) && !is.na(next_val)) (prev_val + next_val) / 2
            else if (!is.na(prev_val)) prev_val
            else if (!is.na(next_val)) next_val
            else NA_real_
  }
  x
}

build_param_df <- function(input) {
  data.frame(
    Parameter = c("Tbase","Topt","Tmax","Theat","Tcold",
                  "FIntMax","CycleLength","TransplantingLag","FloweringLag",
                  "HalfIntGrowth","HalfIntSenescence","InitialInt",
                  "RUE","KcIni","KcMax",
                  "RootIncrease","RootDepthMax","RootDepthInitial",
                  "FieldCapacity","WiltingPoint","DepletionFraction",
                  "SoilWaterInitial","WaterStressSensitivity",
                  "FloweringSlope","FloweringMax",
                  "k0","FruitWaterContentMin","FruitWaterContentMax",
                  "FruitWaterContentInc","FruitWaterContentDecreaseMax"),
    Value = c(input$TGro[[1]], input$Topt, input$TGro[[2]],
              input$TStress[[2]], input$TStress[[1]],
              input$LightInterception[[2]], input$CycleLength,
              input$TransFloLag[[1]], input$TransFloLag[[2]],
              input$GrowthSenescenceCanopy[[1]], input$GrowthSenescenceCanopy[[2]],
              input$LightInterception[[1]],
              input$RUE, input$Kc[[1]], input$Kc[[2]],
              input$RootIncrease, input$RootDepth[[2]], input$RootDepth[[1]],
              0.3, 0.1, input$DepletionFraction,
              input$SoilWaterInitial, input$WaterStressSensitivity,
              input$FloweringSlope, input$FloweringMax,
              input$k0, input$FruitWaterContent[[1]], input$FruitWaterContent[[2]],
              input$FruitWaterContentInc, input$FruitWaterContentDecreaseMax),
    stringsAsFactors = FALSE
  ) |> pivot_wider(names_from = Parameter, values_from = Value)
}

# --- Open-Meteo weather ---------------------------------------------------
# Strategy that GUARANTEES no gap between archive and forecast:
#   1. archive-api  ............  start  .. today - 14
#   2. forecast-api with past_days=14 + forecast_days=16
#                   ............  today - 14 .. today + 16
#   merged + deduplicated by DATE.
# Why: the forecast endpoint with `past_days` reliably backfills the
# last 1..92 days from its own model output, and connects seamlessly
# with the +16 day forecast — no missing days around "now".
.om_daily_pars <- "temperature_2m_max,temperature_2m_min,precipitation_sum"

.om_to_df <- function(resp, is_forecast) {
  d <- resp$daily
  if (is.null(d) || is.null(d$time) || length(d$time) == 0L) return(NULL)
  data.frame(
    DATE        = as.Date(d$time),
    Tx          = as.numeric(d$temperature_2m_max),
    Tn          = as.numeric(d$temperature_2m_min),
    P           = as.numeric(d$precipitation_sum),
    is_forecast = is_forecast,
    stringsAsFactors = FALSE
  )
}

# Archive endpoint: takes start_date/end_date
.om_archive <- function(lon, lat, start, end) {
  httr2::request("https://archive-api.open-meteo.com/v1/archive") |>
    httr2::req_url_query(
      latitude   = lat,  longitude = lon,
      start_date = format(as.Date(start), "%Y-%m-%d"),
      end_date   = format(as.Date(end),   "%Y-%m-%d"),
      daily      = .om_daily_pars,
      timezone   = "auto"
    ) |>
    httr2::req_timeout(45) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

# Forecast endpoint: use past_days/forecast_days (NOT start/end) — robust
# and chiude qualunque buco intorno a "oggi".
.om_forecast <- function(lon, lat, past_days = 14L, forecast_days = 16L) {
  httr2::request("https://api.open-meteo.com/v1/forecast") |>
    httr2::req_url_query(
      latitude       = lat,  longitude = lon,
      past_days      = as.integer(past_days),
      forecast_days  = as.integer(forecast_days),
      daily          = .om_daily_pars,
      timezone       = "auto"
    ) |>
    httr2::req_timeout(45) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}

fetch_openmeteo <- function(lon, lat, start, end) {
  today <- Sys.Date()
  start <- as.Date(start); end <- as.Date(end)
  if (end < start) stop("end < start")

  archive_cutoff <- today - 14L     # piu' generoso: niente gap di confine

  hist_df <- NULL
  if (start <= archive_cutoff) {
    a_end <- min(end, archive_cutoff)
    hist_df <- tryCatch(
      .om_to_df(.om_archive(lon, lat, start, a_end), is_forecast = FALSE),
      error = function(e) {
        warning("Open-Meteo archive: ", conditionMessage(e))
        NULL
      }
    )
  }

  fc_df <- NULL
  if (end > archive_cutoff) {
    fc_df <- tryCatch(
      .om_to_df(.om_forecast(lon, lat, past_days = 14L, forecast_days = 16L),
                is_forecast = TRUE),
      error = function(e) {
        warning("Open-Meteo forecast: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(fc_df)) {
      # Tutto cio' che e' STRETTAMENTE futuro = forecast vero, il resto e'
      # "best estimate" dal modello atmosferico (osservato/quasi-osservato).
      fc_df$is_forecast <- fc_df$DATE >= today
      # Limita al range richiesto
      fc_df <- fc_df[fc_df$DATE >= start & fc_df$DATE <= end, , drop = FALSE]
    }
  }

  out <- dplyr::bind_rows(hist_df, fc_df)
  if (!nrow(out)) stop("Open-Meteo returned no rows")
  # Se archive e forecast si sovrappongono (oggi-14..oggi-14), tieni archive
  out <- out[order(out$DATE, !out$is_forecast), ]
  out <- out[!duplicated(out$DATE), ]
  out <- out[order(out$DATE), ]

  # SANITY CHECK — verifica che non ci siano buchi
  d_seq <- seq(min(out$DATE), max(out$DATE), by = "day")
  missing <- setdiff(as.character(d_seq), as.character(out$DATE))
  if (length(missing)) {
    warning(sprintf("Open-Meteo: %d giorni mancanti (%s..%s).",
                    length(missing), missing[1], tail(missing, 1)))
  }

  out$Lat  <- lat
  out$Site <- sprintf("%.3f_%.3f", lat, lon)
  out
}

# --- Riassunto testuale dei prossimi 3 giorni meteo ---------------------
# Esempio output:
# "Domani sole, max 27°C. Lunedì pioggia (12 mm). Martedì sereno, 28°C."
weather_3day_summary <- function(om, today = Sys.Date()) {
  if (is.null(om) || !nrow(om)) return("Meteo non disponibile.")
  fut <- om[as.Date(om$DATE) > today &
            as.Date(om$DATE) <= today + 3L, , drop = FALSE]
  if (!nrow(fut)) return("Forecast a 3 giorni non disponibile per questo sito.")
  weekday_it <- c("Domenica","Lunedi","Martedi","Mercoledi",
                  "Giovedi","Venerdi","Sabato")
  parts <- vapply(seq_len(nrow(fut)), function(i) {
    d <- as.Date(fut$DATE[i])
    name <- if (as.integer(d - today) == 1L) "Domani"
            else weekday_it[as.POSIXlt(d)$wday + 1L]
    Tx <- fut$Tx[i]; P <- fut$P[i]
    # Soglia "pioggia vera" = .RAIN_DAY_MM (default 10 mm).
    desc <- if (!is.na(P) && P >= .RAIN_DAY_MM) sprintf("pioggia (%.0f mm)", P)
            else if (!is.na(Tx) && Tx >= 32) "caldo intenso"
            else if (!is.na(Tx) && Tx <= 12) "fresco"
            else "sereno"
    sprintf("%s %s, max %.0f\u00b0C", name, desc,
            if (is.na(Tx)) 0 else Tx)
  }, character(1))
  paste0(paste(parts, collapse = ". "), ".")
}

# Convert to the cumba weather schema (Site, Tx, Tn, P, DATE, Lat).
om_to_cumba <- function(om) {
  om |>
    mutate(
      Tx = fill_with_avg(Tx),
      Tn = fill_with_avg(Tn),
      P  = tidyr::replace_na(P, 0)
    ) |>
    select(Site, Tx, Tn, P, DATE, Lat)
}

# --- Reverse geocoding (Nominatim) ---------------------------------------
# Restituisce un nome leggibile per (lat, lon) — es. "Cerignola, FG" o
# "San Severo, Foggia, Apulia". Usa Nominatim (OpenStreetMap), gratuito ma
# con rate limit 1 req/s: chiamiamo solo dopo un clic. In caso di errore
# o rete assente ritorna "" (la UI cade sul fallback "lat/lon").
.NOMINATIM_USER_AGENT <- "CUMBA-Shiny (https://github.com/tomatoModelling/cumba_R_package)"

reverse_geocode_nominatim <- function(lat, lon, lang = "it",
                                      timeout = 10) {
  if (!is.finite(lat) || !is.finite(lon)) return("")
  url <- "https://nominatim.openstreetmap.org/reverse"
  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_url_query(
        format         = "json",
        lat            = format(lat, nsmall = 5L, scientific = FALSE),
        lon            = format(lon, nsmall = 5L, scientific = FALSE),
        zoom           = 12,            # livello "comune"
        addressdetails = 1,
        `accept-language` = lang
      ) |>
      httr2::req_user_agent(.NOMINATIM_USER_AGENT) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
  }, error = function(e) NULL)
  if (is.null(resp)) return("")
  if (httr2::resp_status(resp) >= 400) return("")
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed)) return("")

  a <- parsed$address
  if (is.null(a)) {
    if (!is.null(parsed$display_name) && nzchar(parsed$display_name))
      return(as.character(parsed$display_name))
    return("")
  }
  # Costruisci "Comune (Provincia), Regione" usando i campi piu' utili
  comune <- a$city %||% a$town %||% a$village %||% a$municipality %||%
            a$hamlet %||% a$county %||% ""
  prov   <- a$county %||% a$state_district %||% ""
  reg    <- a$state %||% a$region %||% ""
  paese  <- a$country %||% ""

  parts <- c(
    if (nzchar(comune)) comune else NULL,
    if (nzchar(prov) && prov != comune) prov else NULL,
    if (nzchar(reg) && reg != prov && reg != comune) reg else NULL,
    if (nzchar(paese) && paese != "Italia") paese else NULL
  )
  if (!length(parts) && !is.null(parsed$display_name))
    return(as.character(parsed$display_name))
  paste(parts, collapse = ", ")
}

# --- LLM interpretation (OpenAI-compatible: OpenRouter / Inworld) -------
# Default: OpenRouter con un modello GRATIS (no carta di credito).
# Per ottenere la chiave: https://openrouter.ai/  -> Keys -> Create Key.
# Poi in PowerShell (una volta sola):
#   [Environment]::SetEnvironmentVariable("OPENROUTER_API_KEY","sk-or-...","User")
# e riavvia RStudio.
#
# Per usare Inworld AI Router invece, imposta:
#   LLM_BASE_URL  = "https://api.inworld.ai/llm/v1/openai"
#   LLM_MODEL     = "<id modello inworld>"
#   LLM_API_KEY   = "<la tua chiave inworld>"
#
# Modelli free OpenRouter (al 2026, controlla openrouter.ai/models?supported_parameters=tools&pricing=free):
#   meta-llama/llama-3.3-70b-instruct:free    <-- default, alta qualita'
#   google/gemini-2.0-flash-exp:free
#   deepseek/deepseek-chat:free
#   nvidia/llama-3.1-nemotron-70b-instruct:free
.LLM_BASE_URL <- Sys.getenv("LLM_BASE_URL",
                            "https://openrouter.ai/api/v1")
# Modello PRIMARIO (configurabile via LLM_MODEL).
# Scelto: il MIGLIORE modello free disponibile su OpenRouter al 2026.
# Llama-3.3-70b-instruct ha qualita' molto piu' alta dei free piccoli,
# capisce italiano, segue istruzioni, niente roba "ocr-fast".
.LLM_MODEL    <- Sys.getenv("LLM_MODEL",
                            "meta-llama/llama-3.3-70b-instruct:free")
# Fallback chain: se il modello primario torna 429 (rate limit) o altri 5xx,
# prova il successivo. Lista ordinata per QUALITA' decrescente fra i free
# (verifica/aggiorna su openrouter.ai/models?supported_parameters=tools&pricing=free).
.LLM_FALLBACKS <- unique(c(
  .LLM_MODEL,
  "meta-llama/llama-3.3-70b-instruct:free",
  "deepseek/deepseek-chat-v3.1:free",
  "google/gemini-2.0-flash-exp:free",
  "nvidia/llama-3.1-nemotron-70b-instruct:free",
  "mistralai/mistral-small-3.1-24b-instruct:free",
  "qwen/qwen-2.5-72b-instruct:free"
))

# Risolve la chiave provando in ordine LLM_API_KEY, OPENROUTER_API_KEY,
# INWORLD_API_KEY (nessuna -> messaggio chiaro all'utente).
.llm_resolve_key <- function() {
  for (var in c("LLM_API_KEY", "OPENROUTER_API_KEY", "INWORLD_API_KEY")) {
    k <- Sys.getenv(var, "")
    if (nzchar(k)) return(list(key = k, source = var))
  }
  list(key = "", source = NA_character_)
}

.llm_call_one <- function(model, messages, key, base_url) {
  url <- paste0(base_url, "/chat/completions")
  body <- list(
    model       = model,
    max_tokens  = 500L,
    temperature = 0.4,
    messages    = messages
  )
  resp <- tryCatch({
    req <- httr2::request(url) |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", key),
        "Content-Type"  = "application/json"
      )
    if (grepl("openrouter\\.ai", url)) {
      req <- req |> httr2::req_headers(
        "HTTP-Referer" = "https://github.com/tomatoModelling/cumba_R_package",
        "X-Title"      = "CUMBA Shiny"
      )
    }
    req |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(60) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
  }, error = function(e) structure(list(message = conditionMessage(e)),
                                   class = "cumba_net_err"))

  if (inherits(resp, "cumba_net_err"))
    return(list(ok = FALSE, status = -1L, retryable = TRUE,
                err = sprintf("rete: %s", resp$message)))

  status <- httr2::resp_status(resp)
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)

  if (status >= 400) {
    err <- if (!is.null(parsed$error$message)) parsed$error$message
           else if (!is.null(parsed$message))  parsed$message
           else httr2::resp_body_string(resp)
    # 429 (rate limit) e 5xx -> proveremo il prossimo modello
    retryable <- status == 429L || status >= 500L
    return(list(ok = FALSE, status = status, retryable = retryable,
                err = substr(err, 1, 400)))
  }
  txt <- tryCatch(parsed$choices[[1]]$message$content,
                  error = function(e) NULL)
  if (is.null(txt) || !nzchar(txt))
    return(list(ok = FALSE, status = status, retryable = TRUE,
                err = "risposta vuota"))
  list(ok = TRUE, status = status, text = txt)
}

interpret_with_llm <- function(summary_text, language = "italiano") {
  k <- .llm_resolve_key()
  if (!nzchar(k$key))
    return(structure(paste0(
      "[KEY_MISSING] Chiave LLM non impostata. ",
      "Apri PowerShell e incolla:\n",
      "  [Environment]::SetEnvironmentVariable(\"OPENROUTER_API_KEY\",\"sk-or-...\",\"User\")\n",
      "Poi RIAVVIA RStudio. ",
      "Per ottenere la chiave (gratis): openrouter.ai > Keys > Create Key. ",
      "Per OPZIONE 2 (Inworld), imposta LLM_BASE_URL/LLM_MODEL/LLM_API_KEY in global.R."
    ), class = c("cumba_llm_err", "character")))

  lang_directive <- switch(
    tolower(language),
    "foggiano" = "Rispondi in dialetto foggiano (Apulia, Italia). Caloroso ma preciso.",
    "english"  = "Reply in English. Be precise and concise.",
    "Rispondi in italiano semplice e diretto, da agronomo che parla con un agricoltore."
  )

  system_msg <- paste(
    "Sei un agronomo amico dell'agricoltore di pomodoro da industria.",
    "Hai risultati CUMBA (canopy, yield previsto, brix, stress idrico, FTSW,",
    "fioritura, irrigazioni consigliate, forecast meteo, ensemble di analoghi storici).",
    "Spiega in 4-5 frasi corte, calde e CONCRETE: 1) come sta il campo OGGI",
    "vs anni passati; 2) cosa fare nei prossimi giorni; 3) eventualmente UNA",
    "modifica concreta alla strategia. Niente bullet, niente disclaimer, niente preamboli.",
    lang_directive
  )

  messages <- list(
    list(role = "system", content = system_msg),
    list(role = "user",   content = summary_text)
  )

  # Se l'utente ha customizzato LLM_BASE_URL fuori da OpenRouter
  # (es. Inworld), usa solo .LLM_MODEL senza fallback chain.
  models <- if (grepl("openrouter\\.ai", .LLM_BASE_URL)) .LLM_FALLBACKS
            else .LLM_MODEL

  attempts <- list()
  for (m in models) {
    res <- .llm_call_one(m, messages, k$key, .LLM_BASE_URL)
    attempts[[length(attempts) + 1L]] <- list(model = m,
                                              status = res$status,
                                              err = res$err)
    if (isTRUE(res$ok)) {
      # Se non e' il primo modello, segnalalo discretamente in coda
      tag <- if (m != models[1L])
        sprintf("\n\n[modello: %s — il primario era saturo]", m)
        else ""
      return(paste0(res$text, tag))
    }
    # Se l'errore non e' rate limit / 5xx, smetti subito
    if (!isTRUE(res$retryable)) break
  }

  # Tutti i tentativi falliti: messaggio diagnostico chiaro
  log_lines <- vapply(attempts, function(a) sprintf(
    "  - %s -> HTTP %s: %s", a$model,
    if (is.null(a$status)) "?" else as.character(a$status),
    if (is.null(a$err)) "?" else a$err), character(1))

  last <- attempts[[length(attempts)]]
  hint <- if (!is.null(last$status) && last$status == 429L)
            paste0("[ALL_RATE_LIMITED] Tutti i modelli free di OpenRouter ",
                   "sono saturi al momento. Riprova fra 1-2 minuti, oppure ",
                   "prendi una chiave a pagamento su openrouter.ai ",
                   "(bastano pochi euro per migliaia di richieste).")
          else
            paste0("[LLM_HTTP_", last$status %||% "??", "] ",
                   "Verifica chiave API (OPENROUTER_API_KEY) e connessione.")

  structure(paste0(
    "Errore LLM su ", .LLM_BASE_URL,
    " (key source: ", k$source, ").\nTentativi:\n",
    paste(log_lines, collapse = "\n"), "\n\n", hint
  ), class = c("cumba_llm_err", "character"))
}

# Backward-compat alias (server.R puo' continuare a chiamare il vecchio nome)
interpret_with_claude <- interpret_with_llm

# --- Stato fenologico in italiano (per today-card) -----------------------
# Traduce phenoStage del modello CUMBA in stringhe friendly.
.pheno_it <- function(stage) {
  if (is.null(stage) || is.na(stage)) return(c(label = "—", icon = "—"))
  s <- tolower(as.character(stage))
  switch(s,
    "vegetative"   = c(label = "Fase vegetativa",       icon = "🌱"),
    "flowering"    = c(label = "Fioritura in corso",     icon = "🌼"),
    "fruiting"     = c(label = "Allegagione / frutti",   icon = "🍅"),
    "fruitfilling" = c(label = "Riempimento dei frutti", icon = "🍅"),
    "ripening"     = c(label = "Maturazione",            icon = "🍅"),
    "harvested"    = c(label = "Ciclo concluso",         icon = "✓"),
    c(label = paste("Fase:", stage), icon = "🌿")
  )
}

# --- Today action: cosa fare OGGI sul campo ------------------------------
# Restituisce una list(action, headline, detail, color, icon, phase, phase_icon)
# costruita dal run corrente del modello. Non chiede LLM: e' regole basate sul modello.
today_action <- function(cur, om, transplantingDOY, depletionFraction = 30,
                         today = Sys.Date()) {

  if (is.null(cur) || !nrow(cur)) {
    cur_yr <- as.integer(format(today, "%Y"))
    trans_date <- as.Date(sprintf("%d-01-01", cur_yr)) +
                  (as.integer(transplantingDOY) - 1L)
    if (today < trans_date) {
      n_days <- as.integer(trans_date - today)
      return(list(
        action   = "wait",
        headline = sprintf("Trapianto fra %d giorni", n_days),
        detail   = sprintf("Previsto per %s. La stagione non e' ancora iniziata: prepara il terreno.",
                           format(trans_date, "%d %B")),
        color    = "#6b7480", icon = "🌱",
        phase    = "Pre-trapianto", phase_icon = "🌱"
      ))
    }
    return(list(
      action = "wait",
      headline = "Stagione conclusa o nessun dato",
      detail   = "Imposta una data di trapianto per la stagione corrente.",
      color    = "#6b7480", icon = "—",
      phase    = "—", phase_icon = "—"
    ))
  }

  i_today <- which(as.Date(cur$DATE) == today)
  if (!length(i_today)) i_today <- nrow(cur)  # fallback: ultimo giorno

  # ---- Stato fenologico OGGI -------------------------------------------
  phase_lbl <- "—"; phase_icon <- "🌿"
  if ("phenoStage" %in% names(cur)) {
    p <- .pheno_it(cur$phenoStage[i_today])
    phase_lbl <- unname(p["label"]); phase_icon <- unname(p["icon"])
  }

  irr_today <- as.numeric(cur$irrigation[i_today])
  if (is.na(irr_today)) irr_today <- 0

  ftsw_today <- as.numeric(cur$ftsw[i_today])
  ws_today   <- as.numeric(cur$waterStress[i_today])
  ftsw_threshold <- 1 - as.numeric(depletionFraction) / 100  # sotto questa --> stress

  # Pioggia attesa nei prossimi 3 giorni — conta SOLO i giorni con P >= soglia
  # (gli altri agronomicamente non sostituiscono l'irrigazione: vedi .RAIN_DAY_MM).
  rain_next3 <- 0
  if (!is.null(om) && nrow(om)) {
    om2 <- om[as.Date(om$DATE) > today & as.Date(om$DATE) <= today + 3L, , drop = FALSE]
    if (nrow(om2)) {
      P <- as.numeric(om2$P); P[is.na(P)] <- 0
      rain_next3 <- sum(P[P >= .RAIN_DAY_MM])
    }
  }

  # Prossima irrigazione consigliata (entro 7 giorni)
  fut <- cur[as.Date(cur$DATE) > today & as.Date(cur$DATE) <= today + 7L, , drop = FALSE]
  next_irr_idx <- which(fut$irrigation > 0)[1]

  base <- list(phase = phase_lbl, phase_icon = phase_icon)

  if (irr_today > 0) {
    return(c(list(
      action = "irrigate",
      headline = sprintf("IRRIGA OGGI — %.0f mm", irr_today),
      detail   = sprintf("FTSW = %.2f (soglia stress = %.2f). Pioggia attesa nei prossimi 3 giorni: %.0f mm.",
                         ftsw_today, ftsw_threshold, rain_next3),
      color = "#00838f", icon = "💧"
    ), base))
  }

  # rain_next3 e' gia' filtrato a >= .RAIN_DAY_MM per giorno: se vale almeno
  # una soglia agronomica, la pioggia e' "vera" e sostituisce l'irrigazione.
  if (rain_next3 >= .RAIN_DAY_MM) {
    return(c(list(
      action = "wait_rain",
      headline = sprintf("Aspetta — pioggia attesa: %.0f mm in 3 giorni",
                         rain_next3),
      detail   = sprintf(paste("FTSW oggi = %.2f. Risparmia l'irrigazione:",
                               "la pioggia coprira' il fabbisogno (contiamo",
                               "solo i giorni con >= %.0f mm)."),
                         ftsw_today, .RAIN_DAY_MM),
      color = "#1565c0", icon = "🌧"
    ), base))
  }

  if (!is.na(next_irr_idx)) {
    when <- as.Date(fut$DATE[next_irr_idx])
    days_to <- as.integer(when - today)
    mm <- as.numeric(fut$irrigation[next_irr_idx])
    return(c(list(
      action = "wait_irr",
      headline = sprintf("Prossima irrigazione fra %d giorn%s", days_to,
                         if (days_to == 1L) "o" else "i"),
      detail   = sprintf("Il %s, ~%.0f mm. Oggi FTSW = %.2f, ancora sopra la soglia (%.2f).",
                         format(when, "%a %d %b"), mm, ftsw_today, ftsw_threshold),
      color = "#388e3c", icon = "✅"
    ), base))
  }

  c(list(
    action = "ok",
    headline = "Nessuna irrigazione necessaria a breve",
    detail   = sprintf("FTSW = %.2f (soglia stress %.2f). Stress idrico attuale: %.2f (0=max stress, 1=opt).",
                       ftsw_today, ftsw_threshold, 1 - ws_today),
    color = "#2e7d32", icon = "✅"
  ), base)
}

# --- Suggerisci data di trapianto ----------------------------------------
# Cerca nei prossimi `window_days` la prima data che soddisfa:
#   - pioggia in finestra di 2 giorni (oggi+domani) <= max_rain_2d mm
#   - Tn >= 8 (no gelate)
#   - Tx >= 14 (suolo abbastanza caldo)
# Restituisce as.Date oppure NA se non trovata.
suggest_transplanting_date <- function(om, today = Sys.Date(),
                                       window_days = 30L,
                                       max_rain_2d = 1) {
  if (is.null(om) || !nrow(om)) return(NA)
  fut <- om[as.Date(om$DATE) >= today &
            as.Date(om$DATE) <= today + as.integer(window_days), , drop = FALSE]
  if (nrow(fut) < 2L) return(NA)
  fut <- fut[order(fut$DATE), , drop = FALSE]
  P  <- as.numeric(fut$P);  P[is.na(P)]  <- 0
  Tn <- as.numeric(fut$Tn); Tx <- as.numeric(fut$Tx)
  for (i in seq_len(nrow(fut) - 1L)) {
    rain2 <- P[i] + P[i + 1L]
    if (is.na(Tn[i]) || is.na(Tx[i])) next
    if (rain2 <= max_rain_2d && Tn[i] >= 8 && Tx[i] >= 14)
      return(as.Date(fut$DATE[i]))
  }
  NA
}

# --- Sintesi rule-based (fallback quando l'LLM non risponde) -------------
# Genera un messaggio agronomico in stile "amico che parla", usando solo
# i dati del modello. Volutamente lungo ma scritto in prosa naturale.
synth_message <- function(today_info, om, cur, ens = NULL,
                          today = Sys.Date()) {
  paragraphs <- character()

  # ---- Apertura: fase fenologica + cosa fare oggi ---------------------
  open <- character()
  if (!is.null(today_info$phase) && nzchar(today_info$phase) &&
      today_info$phase != "—" && today_info$phase != "Pre-trapianto") {
    open <- c(open, sprintf("Il campo è in fase %s %s.",
                            today_info$phase_icon, tolower(today_info$phase)))
  } else if (identical(today_info$phase, "Pre-trapianto")) {
    open <- c(open, "Siamo ancora prima del trapianto, la stagione non è iniziata.")
  }
  if (!is.null(today_info$headline)) {
    open <- c(open, paste0(today_info$icon, " ", today_info$headline, "."))
  }
  if (!is.null(today_info$detail)) {
    open <- c(open, today_info$detail)
  }
  if (length(open)) paragraphs <- c(paragraphs, paste(open, collapse = " "))

  # ---- Meteo prossimi 3 gg --------------------------------------------
  if (!is.null(om) && nrow(om)) {
    paragraphs <- c(paragraphs,
                    paste0("Sul meteo: ", weather_3day_summary(om, today)))
  }

  # ---- Proiezione fine ciclo (ensemble) -------------------------------
  if (!is.null(ens) && nrow(ens) &&
      "fruitFreshWeightAct" %in% names(ens) &&
      "template_year" %in% names(ens)) {
    yq <- tapply(ens$fruitFreshWeightAct, ens$template_year, function(x) {
      x <- x[is.finite(x)]
      if (length(x)) tail(x, 1L) / 100 else NA_real_
    })
    yq <- yq[is.finite(yq)]
    if (length(yq) >= 3L) {
      bq <- if ("brixAct" %in% names(ens))
              tapply(ens$brixAct, ens$template_year, function(x) {
                x <- x[is.finite(x)]
                if (length(x)) tail(x, 1L) else NA_real_
              }) else NULL
      bq <- if (!is.null(bq)) bq[is.finite(bq)] else NULL
      brix_part <- if (!is.null(bq) && length(bq) >= 2L)
        sprintf(", brix mediano %.2f° (P10–P90: %.2f–%.2f)",
                quantile(bq, .50, na.rm = TRUE),
                quantile(bq, .10, na.rm = TRUE),
                quantile(bq, .90, na.rm = TRUE))
        else ""
      paragraphs <- c(paragraphs, sprintf(
        paste("Sulla proiezione di fine ciclo, basata su %d analoghi storici:",
              "yield mediana %.1f t/ha (P10–P90: %.1f–%.1f t/ha)%s.",
              "Il ventaglio si stringe man mano che la stagione avanza."),
        length(yq),
        quantile(yq, .50, na.rm = TRUE),
        quantile(yq, .10, na.rm = TRUE),
        quantile(yq, .90, na.rm = TRUE),
        brix_part))
    }
  }

  paste(paragraphs, collapse = "\n\n")
}

# --- Quanto è secco il campo OGGI rispetto agli anni passati? ------------
# Ritorna percentile del FTSW di oggi nella distribuzione degli FTSW dei
# run storici per lo stesso DOY. Esempio: 0.20 => "piu' secco dell'80% degli
# anni passati a questa data".
ftsw_percentile_today <- function(cur, hist_runs, today = Sys.Date()) {
  if (is.null(cur) || is.null(hist_runs) || !nrow(cur) || !nrow(hist_runs))
    return(NA_real_)
  i <- which(as.Date(cur$DATE) == today)
  if (!length(i)) return(NA_real_)
  ftsw_today <- as.numeric(cur$ftsw[i])
  doy_today  <- as.integer(format(today, "%j"))
  hist_at_doy <- hist_runs$ftsw[hist_runs$doy == doy_today]
  hist_at_doy <- hist_at_doy[is.finite(hist_at_doy)]
  if (!length(hist_at_doy)) return(NA_real_)
  mean(hist_at_doy <= ftsw_today, na.rm = TRUE)  # 0..1
}
