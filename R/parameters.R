#' Model Parameters for Cumba
#'
#' This object contains the default model parameters used in the Cumba package.
#' The parameters are loaded from a CSV file and provided as a named list.
#' Each element of the list corresponds to a model parameter and includes its 
#' current value, minimum, maximum, and description.
#'
#' @format A named list where each entry is a list with the following fields:
#' \describe{
#'   \item{description}{Text description of the parameter}
#'   \item{value}{The default value of the parameter}
#'   \item{min}{The minimum allowed value}
#'   \item{max}{The maximum allowed value}
#' }
#'
#' @examples
#' names(cumbaParameters)
#' cumbaParameters$Tbase$value
#'
#' @export
cumbaParameters <- local({
  path <- system.file("extdata", "parameters.csv", package = "cumba")
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  params <- setNames(
    lapply(seq_len(nrow(df)), function(i) {
      list(
        description = df$description[i],
        value = df$value[i],
        min = df$min[i],
        max = df$max[i]
      )
    }),
    df$parameter  # <-- Nomi corretti
  )
  
  params
})
