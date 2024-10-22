#' Query the version of the lessons package
#'
#' @param pkg The name of the lessons package (e.g., "coreRlessons")
#'
#' @return The version of the package, as a character.
#' @export
#'
#' @examples \dontrun{get_lessons_version(pkg = "coreRlessons")}
#'
get_lessons_version <- function(pkg = "coreRlessons", quiet = FALSE) {
  v <- packageVersion(pkg) |> paste(collapse = '.')
  if(!quiet) message(pkg, ' version ', v)
  return(v)
}
