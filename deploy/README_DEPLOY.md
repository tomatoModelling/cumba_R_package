# Deploy CUMBA su shinyapps.io — guida passo passo

**Tempo stimato:** ~15 minuti (la prima volta), ~2 minuti per i deploy successivi.
**Costo:** zero (free tier shinyapps.io = 25 ore-utente al mese).

> ⚡ **TL;DR — primo deploy in 4 mosse**
> 1. Account su shinyapps.io → Tokens → copia name+token+secret
> 2. Metti name+token+secret in `~/.Renviron` come `SHINYAPPS_NAME/TOKEN/SECRET`
> 3. (Opzionale, per LLM) `writeLines("sk-or-...", "shinyApp/openrouter_key.txt")`
> 4. `source("deploy/deploy_to_shinyapps.R")` → aspetta 5–10 min

---

## 1. Prerequisiti (una volta sola)

### 1.1. Account shinyapps.io
1. Vai su <https://www.shinyapps.io> e clicca **Sign up**.
2. Verifica l'email, scegli un *account name* (es. `simone-bregaglio`).
3. In alto a destra clicca sul tuo nome → **Tokens**.
4. Clicca **+ Add Token** → poi **Show secret** → copia il blocco di codice
   tipo:
   ```
   rsconnect::setAccountInfo(name='simone-bregaglio',
                             token='AB...12',
                             secret='ZX...98')
   ```

### 1.2. OpenRouter API key (per l'LLM "CUMBA ti spiega")
La hai già — ricontrolla che sia attiva su
<https://openrouter.ai/keys>. La useremo dopo il primo deploy.

### 1.3. R + RStudio
Devi avere R ≥ 4.2 installato. RStudio non è obbligatorio ma comodo.

---

## 2. Configurare le credenziali (1 volta)

Apri R/RStudio nella cartella del package (`cumba_R_package/`).

Crea o aggiorna il file `~/.Renviron` (è un file *globale* del tuo utente,
non finisce nel repo). Aggiungi 3 righe:

```
SHINYAPPS_NAME=simone-bregaglio
SHINYAPPS_TOKEN=AB...12
SHINYAPPS_SECRET=ZX...98
```

Sostituisci i valori con quelli che hai copiato al punto 1.1.

> **Importante:** il file `.Renviron` non è nel repo Git, quindi i tuoi
> token rimangono privati. Se per sbaglio li metti dentro un file
> committato, **revoca subito il token** dal sito shinyapps.io e creane
> uno nuovo.

Riavvia R per far rileggere `.Renviron`.

---

## 3. Lancia il deploy

Da R/RStudio, con working directory nella cartella del package:

```r
source("deploy/deploy_to_shinyapps.R")
```

Lo script:

1. Installa `rsconnect` e `remotes` (se mancanti).
2. Installa `cumba` da GitHub (`tomatoModelling/cumba_R_package`).
3. Configura l'account shinyapps.io leggendo le env vars.
4. Esegue il pre-flight (lista dei pacchetti R che l'app userà).
5. Carica `shinyApp/` su shinyapps.io. **Questa fase dura 5–10 minuti
   la prima volta** (deve compilare tutte le dipendenze R sul loro
   server). I deploy successivi sono molto più rapidi.

A fine deploy si apre il browser su:
`https://<tuo-nome>.shinyapps.io/cumba/`

---

## 4. Attivare l'LLM (post-deploy, 1 volta)

L'app funziona anche senza LLM (cade su `synth_message()`, regole
deterministiche). Per attivare CUMBA-ti-spiega ci sono **due modi a
seconda del tuo piano** shinyapps.io:

### 4a. Piano FREE (no env vars custom) → file locale

> Sul piano gratuito di shinyapps.io le **Environment Variables custom
> non sono disponibili**. Useremo un file locale che viene caricato
> insieme all'app (e quindi è raggiungibile dal server) ma è in
> `.gitignore` (quindi NON finisce su GitHub).

1. Crea il file `shinyApp/openrouter_key.txt` (una sola riga).
   Da R/RStudio:
   ```r
   writeLines("sk-or-v1-LATUACHIAVE...", "shinyApp/openrouter_key.txt")
   ```
   Oppure aprilo con un editor e incolla solo la chiave.
2. Verifica che `.gitignore` contenga già `shinyApp/openrouter_key.txt`
   (lo fa già lo script di setup di Round 5).
3. Lancia di nuovo `source("deploy/deploy_to_shinyapps.R")`. Il file
   verrà incluso nel bundle e `global.R` lo legge in automatico via
   `.bootstrap_llm_key()`.
4. La chiave è visibile **solo a chi può fare push sul tuo account
   shinyapps.io** (cioè solo te). Non è esposta agli utenti dell'app
   (i file `.txt` non sono pubblicati come risorse statiche).

### 4b. Piano STANDARD ($9/mese) → environment variables

1. Vai su <https://www.shinyapps.io/admin/#/applications>.
2. Clicca sull'app **cumba** → tab **Settings** → sezione
   **Environment Variables** → **+ Add**.
3. Aggiungi:
   - **Name:** `OPENROUTER_API_KEY`
   - **Value:** la tua chiave `sk-or-...`
4. Salva. L'app si riavvia automaticamente.

In entrambi i casi, `global.R` cerca prima la env var, poi il file
`openrouter_key.txt`. Se trovi nessuna, l'app va in fallback rule-based
(badge "REGOLE" accanto al titolo).

(Opzionale) Aggiungi anche `LLM_MODEL` se vuoi forzare un modello
specifico, es. `meta-llama/llama-3.3-70b-instruct:free`.

---

## 5. Aggiornamenti successivi

Dopo aver modificato `shinyApp/` o uno dei file in `R/`:

1. Se hai cambiato `R/Main.R` o altri file del package → committa e fai
   `git push`. Lo script di deploy reinstalla `cumba` da GitHub
   prendendo l'ultimo commit.
2. Lancia di nuovo:
   ```r
   source("deploy/deploy_to_shinyapps.R")
   ```

L'app online viene aggiornata in 2–3 minuti.

---

## 6. Risoluzione problemi più comuni

| Sintomo | Causa probabile | Cosa fare |
|--------|-----------------|----------|
| `Error: account info not set` | Env vars `SHINYAPPS_*` non visibili | Riavvia R; controlla `Sys.getenv("SHINYAPPS_TOKEN")` |
| Deploy si blocca su "compiling..." | Prima volta, compila tutte le dipendenze | Aspetta 5–15 minuti, è normale |
| `Error in install_github: HTTP 404` | Repo cumba privato o nome sbagliato | Rendi il repo pubblico o usa un PAT GitHub |
| App carica ma poi crash | `cumba` non installato lato server | Apri Logs sul cruscotto shinyapps.io |
| LLM non risponde | `OPENROUTER_API_KEY` mancante o scaduta | Punto 4 sopra; rigenera la key se serve |
| "App reached limit of free hours" | Esaurite le 25h/mese del free tier | Riprova il mese dopo, o passa a piano Standard ($9/mese) |

---

## 7. Quanto consumo?

shinyapps.io free = **25 ore-utente attive al mese**. Una "ora-utente" =
un browser aperto sull'app per 1 ora. Quindi 25 utenti diversi che
guardano l'app per 1 ora ciascuno = limite raggiunto.

Per uso interno tra colleghi/aziende agricole è quasi sempre più che
sufficiente. Se diventa virale, valuta:
- **Standard** ($9/mese, 100h, traffico moderato),
- **Hugging Face Spaces** (illimitato, serve Dockerfile),
- **VPS Docker** (~5€/mese, controllo totale).
