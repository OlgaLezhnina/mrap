#' Check that arguments as not missing
#'
#' @param argument An argument of a function
#' @return An error message if missing
#' @noRd
#'
check_argument <- function(argument) {
  if (missing(argument)) {
    stop("Please provide all required arguments")
  }
}

#' Get target_name for parse_code_string function
#'
#' @param argument_string A substring of the code_string
#' @return A target_name as a string, or as a list if more than one
#' @noRd
#
find_target_name <- function(argument_string) {
  if (stringr::str_detect(argument_string, "~")) {
    target_group <- stringr::str_match(argument_string, "(.*)~")[2]
    split_targets <-
      stringr::str_split(target_group, "[(),]")[[1]]
    if (length(split_targets) == 1) {
      target_name <-
        stringr::str_match(split_targets, "(?:.*\\$)?(.*)")[2]
    } else if (split_targets[1] == "cbind") {
      target_name <- as.list(split_targets[2:(length(split_targets) - 1)])
    } else {
      stop("Something went wrong, contact the developers")
    }
  } else {
    splits <- stringr::str_split(argument_string, ",")[[1]]
    second_parts <-
      unique(lapply(splits, function(x) stringr::str_match(x, "\\$(.*)")[2]))
    finals <-  second_parts[!is.na(second_parts)]
    if (length(finals) == 1) {
      target_name <- finals[[1]]
    } else {
      target_name <- NA
    }
  }
  return(target_name)
}

#' Get level_name for parse_code_string function
#'
#' @param argument_string A substring of the code_string
#' @return A level_name as a string
#' @noRd
#
find_level_name <- function(argument_string) {
  if (!stringr::str_detect(argument_string, "[|]")) {
    level_name <- NA
  } else {
    splits <- stringr::str_match_all(argument_string, "[|]([\\w.]+)")[[1]][, 2]
    if (length(splits) == 1) {
      level_name <- splits
    } else {
      level_name <- as.list(stringr::str_split(splits, "\\s+"))
    }
  }
  return(level_name)
}

#' Parse code_string to get names it contains
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @return A named list with strings
#' @noRd
#'
parse_code_string <- function(code_string) {
  if (!is.character(code_string) ||
        !stringr::str_detect(code_string, "::")) {
    stop("Argument code_string should be a string containing package::function")
  }
  without_blanks <-
    stringr::str_replace_all(code_string, " ", "")
  result <- list()
  first_split <-
    stringr::str_match(without_blanks, "([\\w.]+)::([\\w.]+)\\((.+)\\)")
  result[["pack"]] <- first_split[2]
  result[["fun"]] <- first_split[3]
  internal_args <- first_split[4]
  result[["data_name"]] <-
    stringr::str_match(internal_args, "data=([\\w.]+)")[2]
  result[["level_name"]] <- find_level_name(internal_args)
  result[["target_name"]] <- find_target_name(internal_args)
  return(result)
}
