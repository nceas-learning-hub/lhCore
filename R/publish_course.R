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

  ### check whether gh-pages exists; if not, set it up and connect to origin
  setup_gh_pages_branch()

  ### call Quarto command to set up publishing from gh-pages branch
  # system('quarto publish gh-pages --no-prompt')

  ### append a space to index.qmd, then commit it, to force GHA to publish
  write(' ', file = 'index.qmd', append = TRUE)
  system('git add --all')
  system('git commit -m "force GHA initial publication"')
  system('git push')

}

setup_gh_pages_branch <- function() {
  ### set up a gh-pages branch if not already
  branch_check <- system('git branch -a', intern = TRUE) |>
    stringr::str_remove_all('^.+/|^\\* ')
  if(!"gh-pages" %in% branch_check) {
    x <- system('git checkout -b gh-pages')
    # if(x != 0) {
    #   stop('Oops, an error occurred while trying to create gh-pages branch!')
    # }

    ### Connect local to origin
    x <- system('git push -u origin gh-pages')
  }

  ### should we worry about the case where local gh-pages but not connected to origin?

  ### in case there's master but no main...
  main_branch <- ifelse("main" %in% branch_check, "main", "master")

  x <- system(sprintf('git checkout %s', main_branch)) ### set back to main after!
}
