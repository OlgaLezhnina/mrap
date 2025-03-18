#' Title
#'
#' @param dt A datatype loaded with the dtreg package
#' @param code_string a line of code as a string
#' @return a target component instance
#' @noRd
#'
add_target <- function(dt, code_string) {
  parts <- parse_code_string(code_string)
  target_name <- parts$target_name
  if (is.na(target_name)) {
    warning("Target label is not available, you can set it manually",
            call. = FALSE)
  }
  target <- dt$component(label = target_name)
}
