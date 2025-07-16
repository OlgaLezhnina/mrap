#' Write an input instance to be used by other instances
#'
#' @param dt A datatype loaded with the dtreg package
#' @param input_data A data frame, a named list, or a URL as a string
#' @return An input instance
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
    } else {
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
  } else if (is.character(input_data)) {
    inputs <- dt$data_item(source_url = input_data)
  } else {
    stop("Argument data_input should be a dataframe, a named list, or a string")
  }
  return(inputs)
}
