#' Add a software method instance
#'
#' @param pack_fun A string in format "package;function"
#' @param dt A datatype loaded with the dtreg package
#'
#' @return soft_method A soft_method schema instance
#' @noRd
#'
add_soft_method <- function(dt, pack_fun) {
  parts <- strsplit(pack_fun, split = ";")[[1]]
  lib <- parts[[1]]
  fun <- parts[[2]]
  session <- utils::sessionInfo()
  version_r <-
    paste0(session$R.version$major, ".", session$R.version$minor)
  version_lib <- as.character(utils::packageVersion(lib))
  software <- dt$software(label = "R",
                          version_info = version_r)
  soft_library <- dt$software_library(label = lib,
                                      version_info = version_lib,
                                      part_of = software)
  soft_method <- dt$software_method(label = fun,
                                    part_of = soft_library)
  return(soft_method)
}
