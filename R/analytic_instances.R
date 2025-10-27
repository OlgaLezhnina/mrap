#' Create a data_analysis instance
#'
#' @param instances Analytic instance or a list of instances
#' @param code_reference A URL of the code implementing data analysis
#'
#' @return A data analysis instance
#' @export
#'
#' @examples
#' res <- data.frame(mean = 3.758)
#' inst_ds <- descriptive_statistics(
#' "base::mean(iris$Petal.Length)",
#' iris,
#' res
#' )
#' inst_da <- data_analysis(inst_ds)
data_analysis <-
  function(instances, code_reference = NULL) {
    dt <-
      dtreg::load_datatype("https://doi.org/21.T11969/feeb33ad3e4440682a4d")
    data_analysis_inst <- dt$data_analysis(has_part = instances,
                                           is_implemented_by = code_reference)
    return(data_analysis_inst)
  }
#' Create a descriptive_statistics instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A descriptive_statistics instance
#' @export
#'
#' @examples
#' res <- data.frame(mean = 3.758)
#' inst_ds <- descriptive_statistics(
#' "base::mean(iris$Petal.Length)",
#' iris,
#' res
#' )
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

#' Create an algorithm_evaluation instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param named_list_results A named list with metrics and values
#'
#' @return An algorithm_evaluation instance
#' @export
#'
#' @examples
#' res <- list(F1= 0.46, recall = 0.51)
#' inst_ae <- algorithm_evaluation("N/A", "data_url", res)
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

#' Create a multilevel_analysis instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A multilevel_analysis instance
#' @export
#'
#' @examples
#' code_string <- "lme4::lmer(math ~ homework + (1 | schid))"
#' res <- data.frame(result_1 = 1, result_2 = 2)
#' inst <- multilevel_analysis(code_string, "data_url", res)
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

#' Create a correlation_analysis instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A correlation_analysis instance
#' @export
#'
#' @examples
#' res <- data.frame(result_1 = 1, result_2 = 2)
#' inst_ca <- correlation_analysis(
#' "stats::cor.test(iris$Petal.Length, iris$Sepal.Length)",
#' iris,
#' res
#' )
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


#' Create a group_comparison instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A group_comparison instance
#' @export
#'
#' @examples
#' res <- data.frame(result_1 = 1, result_2 = 2)
#' inst_gc <- group_comparison(
#' "stats::aov(Petal.Length ~ Species, data = iris)",
#' iris,
#' res
#' )
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

#' Create a regression_analysis instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A regression_analysis instance
#' @export
#'
#' @examples
#' res <- data.frame(result_1 = 1, result_2 = 2)
#' inst_ra <- regression_analysis(
#' "stats::lm(Petal.Length ~ Sepal.Length, data = iris)",
#' iris,
#' res
#' )
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

#' Create a class_prediction instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A class_prediction instance
#' @export
#'
#' @examples
#' res <- data.frame(result_1 = 1, result_2 = 2)
#' inst_cp <- class_prediction(
#' "stats::glm(Species ~ Petal.Width + Petal.Length, family = 'binomial', data = iris)",
#' iris,
#' res
#' )
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

#' Create a class_discovery instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A class_discovery instance
#' @export
#'
#' @examples
#' clust_data <- iris[-5]
#' res <- data.frame(result_1 = 1, result_2 = 2)
#' inst_cd <- class_discovery(
#' "stats::kmeans(clust_data, 3)",
#' iris,
#' res
#' )
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

#' Create a factor_analysis instance
#'
#' @param code_string A line of code as a string, or "N/A" if not given
#' @param input_data A data frame, a named list, or a URL as a string
#' @param test_results A data frame or a list of data frames
#'
#' @return A factor_analysis instance
#' @export
#'
#' @examples
#' fa_data <- iris[-5]
#' res <- data.frame(result_1 = 1, result_2 = 2)
#' inst_fa <- factor_analysis(
#' "stats::princomp(fa_data)",
#' iris,
#' res
#' )
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
