#' Add a software method instance
#'
#' @param pack The package name as a string
#' @param fun The function name as a string
#'
#' @return soft_method A soft_method schema instance
#' @noRd
#'
add_soft_method <- function(pack, fun) {
  session <- utils::sessionInfo()
  version_R <-
    paste0(session$R.version$major, ".", session$R.version$minor)
  version_pack <- as.character(utils::packageVersion(pack))
  method_name <- paste0(pack, "::", fun)
  software <- dt$software(label = "R",
                          versioninfo = version_R)
  soft_library <- dt$software_library(label = method_name,
                                      versioninfo = version_pack,
                                      part_of = software)
  soft_method <- dt$software_method(label = method_name,
                                    uses_software = soft_library)
  return(soft_method)
}
