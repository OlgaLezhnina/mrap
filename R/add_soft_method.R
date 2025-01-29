#' Add a software method instance
#'
#' @param pack_fun A string in format "package::function"
#' @param dt A datatype loaded with the dtreg package
#'
#' @return soft_method A soft_method schema instance
#' @noRd
#'
add_soft_method <- function(dt, pack_fun) {
  pack <- strsplit(pack_fun, split = "::")[[1]][[1]]
  session <- utils::sessionInfo()
  version_r <-
    paste0(session$R.version$major, ".", session$R.version$minor)
  version_pack <- as.character(utils::packageVersion(pack))
  software <- dt$software(label = "R",
                          versioninfo = version_r)
  soft_library <- dt$software_library(label = pack,
                                      versioninfo = version_pack,
                                      part_of = software)
  soft_method <- dt$software_method(label = pack_fun,
                                    part_of = soft_library)
  return(soft_method)
}
