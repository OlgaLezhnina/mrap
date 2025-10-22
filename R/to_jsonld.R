#' Write an instance in JSON-LD format
#' @description
#' This function is imported from dtreg for ease-of-use
#' @param instance An instance of an R6 class
#' @return JSON string in JSON-LD format
#' @export
#' @examples
#' res <- data.frame(mean = 3.758)
#' inst_ds <- descriptive_statistics(
#' "base::mean(iris$Petal.Length)",
#' iris,
#' res
#' )
#' json <- to_jsonld(inst_ds)
to_jsonld <- dtreg::to_jsonld
