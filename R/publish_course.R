#' Publish course
#'
#' After all the various files are set up, and the course repo is set up with .git
#' and connected to GitHub, it is ready to publish.  The GitHub Action for publishing
#' is already installed during `setup_course_structure()`, but this final command
#' checks to make sure .git and GitHub are enabled, then calls `quarto publish gh-pages --no-prompt`.
#' Note, this could also be done manually in the Terminal.
#'
#' @export
#'
#' @examples \dontrun{publish_course()}
publish_course <- function() {

  ### See if git and github are established!
  git_ready <- check_git_steps()

  if(!git_ready) stop('Course must be git- and github-enabled to publish!')

  ### call Quarto command to set up publishing from gh-pages branch
  system('quarto publish gh-pages --no-prompt')

}
