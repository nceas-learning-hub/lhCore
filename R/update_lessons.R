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
  ### if version tag is NULL use latest otherwise use whatever the user supplied
  ### in the `@ref` portion - could be a commit, tag, branch name
  ### install_github('nceas-learning-hub', 'coreRlessons@v0.1.1')

  ### if version is not current, install coreRlessons from requested version
  ### identify lessons to be updated - which match the names in coreRlessons at this version
  ### copy over into sections folder, overwriting old versions

  ### Set up temp library location for desired version
  tmp_lib <- "~/tmp_coreRlessons"
  dir.create(tmp_lib)

  withr::with_libpaths(tmp_lib,
                       remotes::install_github("nceas-learning-hub/coreRlessons",
                                               ref = "v0.0.1"))

  ### Access files from the package within the temp directory
  xx <- list.files(system.file('lessons', package = "coreRlessons", lib.loc = tmp_lib,
                   mustWork = FALSE), full.names = TRUE)

  ### clean up! (use an on.exit to make sure?)
  unlink(tmp_lib, recursive = TRUE)



}
