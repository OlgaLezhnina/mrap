#' Write a target component instance
#'
#' @param dt A datatype loaded with the dtreg package
#' @param code_string A line of code as a string
#' @return A target component instance
#' @noRd
#'
add_target <- function(dt, code_string) {
  parts <- parse_code_string(code_string)
  target_name <- parts$target_name
  if (is.list(target_name)) {
    target <- list()
    for (name in target_name) {
      target_inst <- dt$component(label = name)
      target <- append(target, target_inst)
    }
  } else {
    if (is.na(target_name)) {
      warning("Target label is not available, you can set it manually",
              call. = FALSE)
    }
    target <- dt$component(label = target_name)
  }
  return(target)
}
