#' Title
#'
#' @param dt  a datatype loaded with the dtreg package
#' @param schema_name the name of an analytic schema as a string
#' @param test_results a resulting data frame, or a list of data frames
#'
#' @return instance of data_item class
#' @noRd
#'
add_generic_output <- function(dt, schema_name, test_results) {
  output <- dt$data_item(label = paste0(schema_name, " results"),
                         source_table = test_results)
  return(output)
}

#' Title
#'
#' @param dt  a datatype loaded with the dtreg package
#' @param named_list_results a named list with metrics and values
#'
#' @return instance of data_item class
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
