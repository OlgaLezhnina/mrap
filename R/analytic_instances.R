#' Title
#'
#' @param code_string a line of code as a string
#' @param input_data an input data frame, or a list of data frames
#' @param test_results a resulting data frame, or a list of data frames
#'
#' @return an instance of group_comparison class
#' @export
#'
#' @examples
group_comparison <- function(code_string, input_data, test_results) {
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  group_comp_inst <-
    write_analytic_instance(dt,
                            "group_comparison",
                            code_string,
                            input_data,
                            test_results)
  group_comp_inst$targets <- add_target(dt, code_string)
  group_comp_inst$has_output <-
    add_generic_output(dt, "group_comparison", test_results)
  return(group_comp_inst)
}

#' Title
#'
#' @param code_string a line of code as a string
#' @param input_data an input data frame, or a list of data frames
#' @param test_results a resulting data frame, or a list of data frames
#'
#' @return an instance of regression_analysis class
#' @export
#'
#' @examples
regression_analysis <-
  function(code_string, input_data, test_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/286991b26f02d58ee490")
    regress_inst <-
      write_analytic_instance(dt,
                              "regression_analysis",
                              code_string,
                              input_data,
                              test_results)
    parts <- parse_code_string(code_string)
    regress_inst$targets <- dt$component(label = parts$target_name)
    regress_inst$has_output <-
      add_generic_output(dt, "regression_analysis", test_results)
    return(regress_inst)
  }
