#' List the available SIA module categories
#'
#' Lists all available categories a SIA module can be placed in. SIA app will
#' place the module with illegal category under "Modules".
#'
#' @return A character vector.
#' @export
#'
#' @family helpers
#'
#' @examples
#' list_categories()
#'
list_categories <- function() {
  c(
    "Scores", "Validity", "Reliability", "Item analysis",
    "Regression", "IRT models", "DIF/Fairness", "Modules"
  )
}
