#' Title
#'
#' @param dt a datatype loaded with the dtreg package
#' @param code_string a line of code as a string
#' @param input_data an input data frame, or a list of data frames
#' @param test_results a resulting data frame, or a list of data frames
#'
#' @return an instance of any analytic class
#' @noRd
#'
write_analytic_instance <-
  function(dt, code_string, input_data, test_results) {
    check_argument(code_string)
    check_argument(input_data)
    check_argument(test_results)
    schema <- dt[length(dt)][[1]]
    schema_name <- as.character(tail(names(dt), 1))
    parts <- parse_code_string(code_string)
    soft_method <- add_soft_method(dt, parts$pack, parts$fun)
    soft_method$is_implemented_by <- code_string
    input <- add_input(dt, input_data)
    input$label  <- parts$data_name
    output <- dt$data_item(label = paste0(schema_name, " results"),
                           source_table = test_results)
    instance <- schema(
      label = schema_name,
      executes = soft_method,
      has_input = input,
      has_output = output
    )
    return(instance)
  }
