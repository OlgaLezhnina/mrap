#' Get the name of the data from an ANOVA object
#' @param aov_object an R object from function aov package stats
#' @return the name of the data as a string
#' @noRd
#'
get_data_name <- function(aov_object) {
  if (toString(aov_object$call$data) == "") {
    data_name <- toString(aov_object$call$formula[[2]][[2]])
  } else {
    data_name <- toString(aov_object$call$data)
  }
  return(data_name)
}

#' Write an instance of R6 class from an ANOVA object
#' @param aov_object an R object from function aov package stats
#' @return R6 class instance
#' @noRd
#'
write_stats_aov <- function(aov_object) {
  var_names <- names(attributes(aov_object$terms)$dataClasses)
  target_name <- var_names[1]
  group_name <- var_names[2]
  data_name <- get_data_name(aov_object)
  sum_object <- summary(aov_object)[[1]]
  dt <-
    dtreg::load_datatype("https://doi.org/21.T11969/b9335ce2c99ed87735a6")
  software_method <- add_software_method(dt, "stats", "aov")
  software_method$is_implemented_by <- deparse(aov_object$call)
  dim_input <-
    dt$matrix_size(
      number_of_rows = nrow(aov_object$model),
      number_of_columns = ncol(aov_object$model)
    )
  input <- dt$data_item(label = data_name,
                        has_characteristic = dim_input)
  target_variable <-
    dt$component(label = target_name)
  output <- dt$data_item(label = "ANOVA results",
                         source_table = data.frame(sum_object))
  instance <- dt$group_comparison(
    label = paste0("Anova ", target_name, " vs ", group_name),
    executes = software_method,
    has_input = input,
    targets = target_variable,
    has_output = output
  )
  return(instance)
}

#' Wrap stats::aov function
#' @param ... the same arguments as in the wrapped function
#' @return a list of ANOVA object and R6 class instance
#' @export
#'
#' @examples
#' results <- stats_aov(Petal.Length ~ Species, data = iris)
#'
stats_aov <- function(...) {
  call <- match.call()
  aov_object <- stats::aov(...)
  aov_object$call <- call
  dtreg_object <- write_stats_aov(aov_object)
  result <- list(aov_object, dtreg_object)
  names(result) <- c("anova", "dtreg_object")
  return(result)
}
