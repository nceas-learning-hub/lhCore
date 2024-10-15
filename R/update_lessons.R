#' Update lessons in a coreR course
#'
#' @param lessons A vector of specific lessons to be updated, based on file
#'     names (bare or with `.qmd` extension).  If lesson names don't match
#'     lessons in the source book, an error will be returned.  The default `all`
#'     will update all available lessons, i.e., those whose lesson names match
#'     those in the source book.  Customized lessons, whose names do not match
#'     those in the source book, will not be updated, even with `version = 'all'`.
#' @param version A version of the coreR package from which the updated lessons
#'     should be pulled.  `version` can be given as a commit hash, a tag, a
#'     version, or a date.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # update_lessons(lessons = c('git-github-intro', 'r-creating-packages.qmd'),
#' #                version = '0.0.0.9000')

update_lessons <- function(lessons = "all", version = NULL) {
  ### placeholder - give a vector of lesson names, optionally give a version tag
  ### if version tag is NULL use latest
  ### maybe version tag is a commit hash, a date, a version number, or a release?
  ### could also give a flag of lessons = 'all' maybe, so it updates all update-able lessons
  ### is there a way to track which lessons are custom (and not update those)?
  ### * maybe locally, make a copy with a different name?
  ### * if filename doesn't match an "official" lesson then it won't get updated (with a warning)
}
