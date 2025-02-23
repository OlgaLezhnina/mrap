#' Title
#'
#' @param instance an instance of dtreg R6class
#' @param jsonld Boolean whether the output should be JSON-LD string
#'
#' @return either an instance or its JSON-LD string
#' @noRd
#'
assign_result <- function(instance, jsonld) {
  if (jsonld) {
    instance <- dtreg::to_jsonld(instance)
  }
  return(instance)
}
