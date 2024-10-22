#' Query the version of the lessons package
#'
#' @param pkg The name of the lessons package (e.g., "coreRlessons")
#'
#' @return The version of the package, as a character.
#' @export
#'
#' @examples \dontrun{get_lessons_version(pkg = "coreRlessons")}
#'
get_lessons_version <- function(pkg = "coreRlessons") {

    installed <- as.data.frame(installed.packages())
    pkg_installed <- installed[installed$Package == pkg, ]

    if(nrow(pkg_installed) == 0) {
      stop("No lessons package detected - please check the package name, or install!  ",
           "\n  e.g., remotes::install_github(\"nceas-learning-hub/coreRlessons\")")
    }
    if(nrow(pkg_installed) > 1) stop("multiple packages detected???")

    return(pkg_installed$Version)
}
