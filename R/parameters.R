#' Model Parameters for Cumba
#'
#' This dataset contains the default model parameters used in the Cumba package.
#' Each element corresponds to a model parameter and includes its description, 
#' current value, minimum, and maximum.
#'
#' @format A named list where each entry is a list with:
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
#' 
"cumbaParameters"
