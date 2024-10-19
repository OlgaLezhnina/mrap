#' Select test of difference schema from a dtr
#' @description
#' Select the schema with statistical test of difference
#' from a supported data type registry, which is currently the ePIC or the ORKG.
#' @param dtr A string specifying a data type registry
#' @return A list with the test of difference schema from the specified dtr
#' @noRd
#'
select_stat_test_diff <- function(dtr) {
  if (dtr == "epic") {
    stat_test_diff <-
      dtreg::load_datatype("https://doi.org/21.T11969/ff5e3f857788d20dd1aa")
  } else if (dtr == "orkg") {
    stat_test_diff <-
      dtreg::load_datatype("https://incubating.orkg.org/template/R836000")
  } else {
    stop("The dtr can be only epic or orkg")
  }
  return(stat_test_diff)
}
