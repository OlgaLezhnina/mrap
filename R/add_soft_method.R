#' Add a software method instance
#'
#' @param method_name A string with the name of a software method
#'
#' @return soft_method A sub-schema instance
#' @noRd
#'
add_soft_method <- function(method_name) {
  session <- utils::sessionInfo()
  version <-
    paste0(session$R.version$major, ".", session$R.version$minor)
  soft_all <-
    dtreg::load_datatype("https://doi.org/21.T11969/abef9f1c0d48853f3e51")
  software <- soft_all$software(label = "R",
                                versioninfo = version)
  soft_library <- soft_all$software_library(label = method_name,
                                            part_of = software,
                                            version_info = version)
  soft_method <- soft_all$software_method(
    label = method_name,
    uses_software = soft_library
  )
  return(soft_method)
}
