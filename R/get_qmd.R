#' Copy a lesson file from the package to the location
#'
#' @param lesson_id The bare filename of the lesson (without file extension)
#' @param loc The location on the local computer where the file should be copied
#'
#' @return TRUE if the file copied successfully
#' @export
#'
#' @examples
#' # get_qmd('git-github-intro')

get_qmd <- function(lesson_id, loc = ".") {
  lesson_src <- system.file("sections", paste0(lesson_id, ".qmd"), package = 'coreR')
  lesson_out <- file.path(loc, paste0(lesson_id, ".qmd"))
  if(!file.exists(lesson_src)) stop("Oh no! ", lesson_id, " does not seem to be a valid lesson name!")
  success <- file.copy(lesson_src, lesson_out)

  if(!success) stop("Oh no, the file didn't copy properly!")
  return(success)
}
