#' Query the version of the lessons package
#'
#' @param pkg The name of the lessons package (e.g., "lhLessons") currently installed.
#' @param quiet Suppress display of the version as a message?
#'
#' @return The version of the package, as a character.
#' @export
#'
#' @examples \dontrun{get_lessons_version(pkg)}
#'
get_lessons_version <- function(pkg, quiet = FALSE) {
  v <- utils::packageDescription(pkg, fields = "Version")
  if(!quiet) message(pkg, ' version ', v)
  return(v)
}
