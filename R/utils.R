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
  if (!is.character(code_string) ||
        !stringr::str_detect(code_string, "::")) {
    stop("Argument code_string should be a string containing package::function")
  } else {
    result <- list()
    first_split <-
      stringr::str_match(code_string, "([\\w.]+)::([\\w.]+)\\((.+)\\)")
    result["pack"] <- first_split[2]
    result["fun"] <- first_split[3]
    internal_args <- first_split[4]
    result["data_name"] <-
      stringr::str_match(internal_args, "data\\s*=\\s*([\\w.]+)")[2]
    result["level_name"] <-
      stringr::str_match(internal_args, "\\|\\s*([\\w.]+)")[2]
    if (stringr::str_detect(internal_args, "~")) {
      result["target_name"] <- "TODO1"
    } else {
      result["target_name"] <- "TODO2"
    }
  }
  return(result)
}
