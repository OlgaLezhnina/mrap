#' Add a software method instance
#'
#' @param dt A datatype loaded with the dtreg package
#' @param code_string A line of code as a string
#'
#' @return A software_method instance
#' @noRd
#'
add_software_method <- function(dt, code_string) {
  session <- utils::sessionInfo()
  version_r <-
    paste0(session$R.version$major, ".", session$R.version$minor)
  software <- dt$software(
    label = "R",
    version_info = version_r,
    has_support_url = "https://www.r-project.org/"
  )
  if (code_string == "N/A") {
    software_method <- dt$software_method(part_of = software)
  } else {
    pack <- parse_code_string(code_string)[["pack"]]
    fun <-  parse_code_string(code_string)[["fun"]]
    version_pack <- as.character(utils::packageVersion(pack))
    software_library <- dt$software_library(label = pack,
                                            version_info = version_pack,
                                            part_of = software)
    software_method <- dt$software_method(label = fun,
                                          part_of = software_library)
    software_method$is_implemented_by <- code_string
  }
  return(software_method)
}
