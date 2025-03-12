#' Title
#'
#' @param dt A datatype loaded with the dtreg package
#' @param input_data a dataframe or a list
#' @return an instance of input class
#' @noRd
#'
add_input <- function(dt, input_data) {
  if (is.data.frame(input_data)) {
    dimensions_input <-
      dt$matrix_size(
        number_of_rows = nrow(input_data),
        number_of_columns = ncol(input_data)
      )
    inputs <- dt$data_item(has_characteristic = dimensions_input)
  } else if (is.list(input_data)) {
    if (is.null(names(input_data))) {
      stop("You input_data list should be named. See 'named list in R'.")
    } else{
      inputs <- list()
      for (name in names(input_data)) {
        dimensions_inp <- dt$matrix_size(
          number_of_rows = nrow(input_data[[name]]),
          number_of_columns = ncol(input_data[[name]])
        )
        inp_inst <- dt$data_item(label = name,
                                 has_characteristic = dimensions_inp)
        inputs <- append(inputs, inp_inst)
      }
    }
  } else {
    stop("Argument data_input should be either a dataframe or a list")
  }
  return(inputs)
}
