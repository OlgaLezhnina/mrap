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
descriptive_statistics <-
  function(code_string, input_data, test_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/5b66cb584b974b186f37")
    descriptive_stats_inst <-
      write_analytic_instance(dt,
                              "descriptive_statistics",
                              code_string,
                              input_data)
    descriptive_stats_inst$has_output <-
      add_generic_output(dt, "descriptive_statistics", test_results)
    return(descriptive_stats_inst)
  }

#' Title
#'
#' @param code_string a line of code as a string
#' @param input_data an input data frame, or a list of data frames
#' @param named_list_results a named list with metrics and values
#'
#' @return an instance of group_comparison class
#' @export
#'
#' @examples
algorithm_evaluation <-
  function(code_string,
           input_data,
           named_list_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/5e782e67e70d0b2a022a")
    algorithm_evaluation_inst <-
      write_analytic_instance(dt,
                              "algorithm_evaluation",
                              code_string,
                              input_data)
    algorithm_evaluation_inst$has_output <-
      add_evaluation_output(dt, named_list_results)
    return(algorithm_evaluation_inst)
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
multilevel_analysis <-
  function(code_string, input_data, test_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/c6b413ba96ba477b5dca")
    mult_analysis_inst <-
      write_analytic_instance(dt,
                              "multilevel_analysis",
                              code_string,
                              input_data)
    parts <- parse_code_string(code_string)
    mult_analysis_inst$level <- dt$component(label = parts$level_name)
    mult_analysis_inst$targets <- dt$component(label = parts$target_name)
    mult_analysis_inst$has_output <-
      add_generic_output(dt, "multilevel_analysis", test_results)
    return(mult_analysis_inst)
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
correlation_analysis <-
  function(code_string, input_data, test_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/3f64a93eef69d721518f")
    corr_analysis_inst <-
      write_analytic_instance(dt,
                              "correlation_analysis",
                              code_string,
                              input_data)
    corr_analysis_inst$has_output <-
      add_generic_output(dt, "correlation_analysis", test_results)
    return(corr_analysis_inst)
  }


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
  group_comparison_inst <-
    write_analytic_instance(dt,
                            "group_comparison",
                            code_string,
                            input_data)
  group_comparison_inst$targets <- add_target(dt, code_string)
  group_comparison_inst$has_output <-
    add_generic_output(dt, "group_comparison", test_results)
  return(group_comparison_inst)
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
    regr_analysis_inst <-
      write_analytic_instance(dt,
                              "regression_analysis",
                              code_string,
                              input_data)
    parts <- parse_code_string(code_string)
    regr_analysis_inst$targets <-
      dt$component(label = parts$target_name)
    regr_analysis_inst$has_output <-
      add_generic_output(dt, "regression_analysis", test_results)
    return(regr_analysis_inst)
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
class_prediction <-
  function(code_string, input_data, test_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/6e3e29ce3ba5a0b9abfe")
    class_prediction_inst <-
      write_analytic_instance(dt,
                              "class_prediction",
                              code_string,
                              input_data)
    parts <- parse_code_string(code_string)
    class_prediction_inst$targets <-
      dt$component(label = parts$target_name)
    class_prediction_inst$has_output <-
      add_generic_output(dt, "class_prediction", test_results)
    return(class_prediction_inst)
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
class_discovery <-
  function(code_string, input_data, test_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/c6e19df3b52ab8d855a9")
    class_discovery_inst <-
      write_analytic_instance(dt,
                              "class_discovery",
                              code_string,
                              input_data)
    class_discovery_inst$has_output <-
      add_generic_output(dt, "class_discovery", test_results)
    return(class_discovery_inst)
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
factor_analysis <-
  function(code_string, input_data, test_results) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/437807f8d1a81b5138a3")
    factor_analysis_inst <-
      write_analytic_instance(dt,
                              "factor_analysis",
                              code_string,
                              input_data)
    factor_analysis_inst$has_output <-
      add_generic_output(dt, "factor_analysis", test_results)
    return(factor_analysis_inst)
  }
