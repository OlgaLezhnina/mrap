#' Write a generic output instance to be used by other instances
#'
#' @param dt  A datatype loaded with the dtreg package
#' @param schema_name The name of an analytic schema as a string
#' @param test_results A resulting data frame, or a list of data frames
#'
#' @return A generic output instance
#' @noRd
#'
add_generic_output <- function(dt, schema_name, test_results) {
  output <- dt$data_item(label = paste0(schema_name, " results"),
                         source_table = test_results)
  return(output)
}

#' Write an algorithm_evaluation output instance to be used by other instances
#'
#' @param dt  A datatype loaded with the dtreg package
#' @param named_list_results A named list with metrics and values
#'
#' @return An algorithm_evaluation output instance
#' @noRd
#'
add_evaluation_output <- function(dt, named_list_results) {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/5e782e67e70d0b2a022a")
  dataframe_results <- data.frame(named_list_results)
  rownames(dataframe_results) <- "value"
  output <- dt$data_item(label = "algorithm evaluation results",
                         source_table = dataframe_results)
  return(output)
}
