#' Query the tags of the lessons package remote
#'
#' @param pkg The name of the lessons package repository (e.g., "lhLessons")
#' @param org The GitHub organization where the lessons package repo lives.
#'        Default "nceas-learning-hub"
#' @param latest Return only the latest tag?  Default `FALSE` returns all tags.
#' @param quiet Suppress display of the version as a message?
#'
#' @return Vector of tags for the package, as characters.
#' @export
#'
#' @examples \dontrun{query_lessons_tags(pkg = 'lhLessons', latest = TRUE)}
#'
query_lessons_tags <- function(pkg, org = 'nceas-learning-hub',
                             latest = FALSE,
                             quiet  = FALSE) {
  sys_stem <- 'git ls-remote --tags https://github.com/%s/%s'
  sys_str  <- sprintf(sys_stem, org, pkg)
  sys_out <- system(sys_str, intern = TRUE)
  tags_vec <- sub('^.+refs/tags/', sys_out)
  if(!quiet) message(pkg, ' tags: \n', paste0('   ', tags_vec, collapse = '\n'))
  if(latest) {
    latest <- tags_vec[grepl('^20[0-9]{2}.[0-9]{2}.[0-9]{2}$', tags_vec)] |>
      max()
    return(latest)
  }
  return(tags_vec)
}
