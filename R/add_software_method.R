#' Add a software method instance
#'
#' @param dt A datatype loaded with the dtreg package
#' @param pack A string package name
#' @param fun A string function name
#'
#' @return software_method A software_method schema instance
#' @noRd
#'
add_software_method <- function(dt, pack, fun) {
  session <- utils::sessionInfo()
  version_r <-
    paste0(session$R.version$major, ".", session$R.version$minor)
  version_pack <- as.character(utils::packageVersion(pack))
  software <- dt$software(label = "R",
                          version_info = version_r)
  software_library <- dt$software_library(label = pack,
                                          version_info = version_pack,
                                          part_of = software)
  software_method <- dt$software_method(label = fun,
                                        part_of = software_library)
  return(software_method)
}
