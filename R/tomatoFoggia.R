#' Tomato field trial data from Foggia (Italy)
#'
#' A dataset containing weather, irrigation, management, and production data 
#' from processing tomato field trials conducted near Foggia (Apulia, Italy).
#' The dataset is organized as a list of four data frames.
#'
#' @format A list of four data frames:
#' \describe{
#'   \item{\code{weather}}{Daily weather data with columns:
#'     DATE (POSIXct), Tx (max temp, °C), Tn (min temp, °C),
#'     P (precipitation, mm), Rad (radiation, MJ m-2 d-1),
#'     RHx (max RH, %), RHn (min RH, %), W (wind speed, m s-1),
#'     Date (Date), Site ("Foggia"), Lat (latitude, numeric).}
#'
#'   \item{\code{irrigation}}{Irrigation records with columns:
#'     ID (plot id), DATE (POSIXct), WVOL (mm), YEAR,
#'     CV (cultivar), TRANS (transplanting, POSIXct),
#'     HARV (harvest, POSIXct), IRR_M (management, e.g. "50%"),
#'     Site ("Foggia").}
#'
#'   \item{\code{management}}{Plot identifiers and treatments with columns:
#'     ID, YEAR, CV, TRANS (POSIXct), HARV (POSIXct), IRR_M.}
#'
#'   \item{\code{production}}{Production and quality data with columns:
#'     ID, yield_ref (t ha-1), yield_ref_sd (t ha-1),
#'     brix_ref (°Brix), brix_ref_sd (°Brix).}
#' }
#'
#' @source Carucci et al. (2024) \url{https://doi.org/10.1016/j.dib.2024.110225}
#'
#' @examples
#' data(tomatoFoggia)
#' str(tomatoFoggia)
#' head(tomatoFoggia$weather)
#'
"tomatoFoggia"

