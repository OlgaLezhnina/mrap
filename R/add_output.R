#' Title
#'
#' @param dt  a datatype loaded with the dtreg package
#' @param schema_name the name of an analytic schema as a string
#' @param test_results a resulting data frame, or a list of data frames
#'
#' @return instance of data_item class
#' @noRd
#'
add_output <- function(dt, schema_name, test_results) {
  output <- dt$data_item(label = paste0(schema_name, " results"),
                         source_table = test_results)
  return(output)
}
