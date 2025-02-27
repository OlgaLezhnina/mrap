#' Title
#'
#' @param dt A datatype loaded with the dtreg package
#' @param input_data a dataframe
#' @return an instance of input class
#' @noRd
#'
add_input <- function(dt, input_data) {
  dim_input <-
    dt$matrix_size(
      number_of_rows = nrow(input_data),
      number_of_columns = ncol(input_data)
    )
  input <- dt$data_item(has_characteristic = dim_input)
  return(input)
}
