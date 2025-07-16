#' Write a level component instance
#'
#' @param dt A datatype loaded with the dtreg package
#' @param code_string A line of code as a string
#' @return A level component instance
#' @noRd
#'
add_level <- function(dt, code_string) {
  parts <- parse_code_string(code_string)
  level_name <- parts$level_name
  if (is.list(level_name)) {
    level <- list()
    for (name in level_name) {
      level_inst <- dt$component(label = name)
      level <- append(level, level_inst)
    }
  } else {
    if (is.na(level_name)) {
      warning("Level label is not available, you can set it manually",
              call. = FALSE)
    }
    level <- dt$component(label = level_name)
  }
  return(level)
}
