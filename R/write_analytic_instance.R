#' Write a generic part of analytic instances
#'
#' @param dt A datatype loaded with the dtreg package
#' @param schema_name An analytic schema name as a string
#' @param code_string A line of code as a string or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#'
#' @return A generic instance to be used by any analytic instance
#' @noRd
#'
write_analytic_instance <-
  function(dt,
           schema_name,
           code_string,
           input_data) {
    check_argument(code_string)
    check_argument(input_data)
    software_method <-
      add_software_method(dt, code_string)
    input <- add_input(dt, input_data)
    if (is.data.frame(input_data)) {
      parts <- parse_code_string(code_string)
      input$label  <- parts$data_name
    }
    instance <- dt[[schema_name]](label = schema_name,
                                  executes = software_method,
                                  has_input = input)
    return(instance)
  }
