#' Title
#'
#' @param argument an argument of a function
#' @return an error message or none
#' @noRd
#'
check_argument <- function(argument) {
  if (missing(argument)) {
    stop("Please provide all required arguments")
  }
}

#' Title
#'
#' @param code_string a line of code as a string
#' @return a named list with substrings
#' @noRd
#'
parse_code_string <- function(code_string) {
  result <- list()
  groups <- "([\\w.]+)::([\\w.]+)\\(\\s*(.+?)\\s*~\\s*(.+)\\)"
  string_info <- stringr::str_match(code_string, groups)
  result["pack"] <- string_info[2]
  result["fun"] <- string_info[3]
  result["target_name"] <- string_info[4]
  result["label_name"] <- "todo"
  result["data_name"] <- "change_this"
  return(result)
}

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
