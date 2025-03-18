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
#' @param argument_string a substring of the code_string
#' @return target_name as a string
#' @noRd
#
find_target_name <- function(argument_string) {
  without_blanks <-
    stringr::str_replace_all(argument_string, fixed(" "), "")
  if (stringr::str_detect(without_blanks, "~")) {
    target_group <- stringr::str_match(without_blanks, "(.*)~")[2]
    split_targets <-
      stringr::str_split(target_group, "\\(|,|\\)")[[1]]
    if (length(split_targets) == 1) {
      if (stringr::str_detect(split_targets, "\\$")) {
        target_name <- stringr::str_match(split_targets, "\\$(.*)")[2]
      } else {
        target_name <- split_targets
      }
    } else if (split_targets[1] == "cbind") {
      target_name <- "TODO with a list"
    } else {
      stop("Something went wrong, contact the developers")
    }
  } else {
    splits <- stringr::str_split(without_blanks, ",")[[1]]
    if (!is.na(stringr::str_match(splits[1], "\\$(.*)")[2]) &&
          stringr::str_match(splits[1], "\\$(.*)")[2] ==
            stringr::str_match(splits[2], "\\$(.*)")[2]) {
      target_name <- stringr::str_match(splits[1], "\\$(.*)")[2]
    } else {
      target_name <- NA
    }
  }
  return(target_name)
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
    result["target_name"] <- find_target_name(internal_args)
  }
  return(result)
}
