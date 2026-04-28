# =====================================================================
# deploy/deploy_to_shinyapps.R
# -----------------------------------------------------------------------
# Pubblica la Shiny app cumba su https://www.shinyapps.io
#
# COSA FA QUESTO SCRIPT (in ordine):
#   1. Installa (se mancanti) i pacchetti `rsconnect` e `remotes`.
#   2. Installa il package `cumba` dal repo GitHub
#      tomatoModelling/cumba_R_package -> serve perche' shinyapps.io
#      usera' la versione installata (non e' un pacchetto su CRAN).
#   3. Configura il tuo account shinyapps.io leggendo TOKEN/SECRET dalle
#      variabili d'ambiente SHINYAPPS_TOKEN, SHINYAPPS_SECRET, SHINYAPPS_NAME.
#      (NON committare mai le credenziali nel repo.)
#   4. Lancia rsconnect::deployApp() puntando alla cartella ../shinyApp.
#
# COME OTTENERE LE CREDENZIALI shinyapps.io (1 volta sola, ~2 minuti):
#   a. Vai su https://www.shinyapps.io e accedi (gratuito).
#   b. In alto a destra: clicca sul tuo nome -> "Tokens".
#   c. Clicca "Add Token" -> "Show secret" -> copia la riga che ti propone.
#      (E' un comando rsconnect::setAccountInfo(name=..., token=..., secret=...).)
#   d. Estrai i 3 valori e mettili in queste env vars (es. nel terminale R):
#         Sys.setenv(SHINYAPPS_NAME   = "tuo-nome-account")
#         Sys.setenv(SHINYAPPS_TOKEN  = "ABC...123")
#         Sys.setenv(SHINYAPPS_SECRET = "xyz...456")
#      Oppure mettile in ~/.Renviron (NON committare quel file).
#
# DOPO IL DEPLOY:
#   - L'app sara' su  https://<account>.shinyapps.io/cumba/
#   - Per attivare l'LLM, vai sul cruscotto shinyapps.io:
#       Applications -> cumba -> Settings -> Environment Variables
#       Aggiungi:  OPENROUTER_API_KEY = sk-or-...
#     (NON va committata nel repo.)
#
# =====================================================================

# ---- 1) Pacchetti --------------------------------------------------------
need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
need("rsconnect")
need("remotes")

# ---- 2) Installa cumba da GitHub ----------------------------------------
# Cosi' rsconnect, quando scopre che l'app fa library(cumba), lo registra
# nel manifest con il commit GitHub corrente (lo SHA viene serializzato).
cat("\n[1/4] Installo cumba da GitHub...\n")
remotes::install_github("tomatoModelling/cumba_R_package",
                        upgrade = "never", quiet = FALSE)

# Quick sanity check: il package si carica?
ok <- suppressMessages(suppressWarnings(
  tryCatch({ library(cumba); TRUE }, error = function(e) FALSE)
))
if (!ok) stop("Il package 'cumba' non si carica dopo l'install. ",
              "Controlla i log sopra.")

# ---- 3) Account shinyapps.io --------------------------------------------
cat("\n[2/4] Configuro l'account shinyapps.io...\n")
acc_name   <- Sys.getenv("SHINYAPPS_NAME",   "")
acc_token  <- Sys.getenv("SHINYAPPS_TOKEN",  "")
acc_secret <- Sys.getenv("SHINYAPPS_SECRET", "")

if (nzchar(acc_name) && nzchar(acc_token) && nzchar(acc_secret)) {
  rsconnect::setAccountInfo(name   = acc_name,
                            token  = acc_token,
                            secret = acc_secret)
  cat("  -> account '", acc_name, "' configurato.\n", sep = "")
} else {
  message("ATTENZIONE: SHINYAPPS_NAME/TOKEN/SECRET non impostate.\n",
          "Se non hai mai chiamato rsconnect::setAccountInfo() prima,\n",
          "il deploy fallira'. Vedi le istruzioni in cima a questo script.")
}

# ---- 4) Deploy -----------------------------------------------------------
cat("\n[3/4] Pre-flight: dipendenze rilevate dall'app...\n")

# Trova la cartella shinyApp/. Cerchiamo in (in ordine):
#   1. <getwd()>/shinyApp           (caso classico: source da package root)
#   2. <getwd()>/../shinyApp        (caso: source da deploy/ stessa)
#   3. parent del file di script    (se 'this.path' e' disponibile)
candidates <- c(
  file.path(getwd(), "shinyApp"),
  file.path(getwd(), "..", "shinyApp")
)
if (requireNamespace("this.path", quietly = TRUE)) {
  here <- tryCatch(this.path::this.dir(), error = function(e) NA_character_)
  if (!is.na(here))
    candidates <- c(candidates, file.path(here, "..", "shinyApp"))
}
app_dir <- NULL
for (cand in candidates) {
  if (file.exists(file.path(cand, "global.R"))) {
    app_dir <- normalizePath(cand, mustWork = TRUE)
    break
  }
}
if (is.null(app_dir))
  stop("Non trovo la cartella shinyApp/. Lancia lo script avendo come\n",
       "  working directory la root del package (la cartella che contiene\n",
       "  DESCRIPTION e shinyApp/), poi:  source('deploy/deploy_to_shinyapps.R').")
cat("  appDir = ", app_dir, "\n", sep = "")

# rsconnect rileva automaticamente i pacchetti R caricati dall'app
# (library/require/::) tramite static analysis dei sorgenti.
deps <- tryCatch(
  rsconnect::appDependencies(appDir = app_dir),
  error = function(e) NULL
)
if (!is.null(deps)) {
  cat("  pacchetti rilevati: ", nrow(deps), "\n", sep = "")
  # rsconnect cambia colonne fra versioni; mostriamo SOLO quelle che esistono
  # davvero nel data.frame restituito (a volte sono Package/Version/Source,
  # a volte package/version/source, a volte vuoto del tutto).
  cols_show <- intersect(c("Package", "package", "Version", "version",
                           "Source",  "source"),
                         names(deps))
  if (length(cols_show)) {
    print(head(deps[, cols_show, drop = FALSE], 30L), row.names = FALSE)
    if (nrow(deps) > 30L)
      cat("  ... e altri ", nrow(deps) - 30L, " pacchetti.\n", sep = "")
  }
}

cat("\n[4/4] Deploy in corso (puo' richiedere 5-10 minuti)...\n")
rsconnect::deployApp(
  appDir       = app_dir,
  appName      = "cumba",
  appTitle     = "CUMBA — pomodoro da industria",
  account      = if (nzchar(acc_name)) acc_name else NULL,
  forceUpdate  = TRUE,
  launch.browser = TRUE
)

cat("\nFINE. Vai su https://", acc_name, ".shinyapps.io/cumba/ per provare.\n",
    sep = "")
cat("PROMEMORIA: imposta OPENROUTER_API_KEY nelle Environment Variables\n",
    "su https://www.shinyapps.io/admin/#/applications (Settings dell'app).\n",
    sep = "")
