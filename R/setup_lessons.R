#' Set up a schedule of course lessons
#'
#' @param lessons A character vector containing the names (bare or .qmd extension) of the lessons, in order.
#' @param overwrite If there is already an existing lessons structure, and overwrite == FALSE, will return an error.  If the user wishes to create a new lesson structure, then overwrite == FALSE will remove the existing structure and create a new structure based on the lesson names.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # setup_lessons('git-github-intro', 'r-creating-functions.qmd')

setup_lessons <- function(lessons, overwrite = FALSE) {
  ### This will set up the lesson structure
  ### - take in a vector of lesson names in order
  ### - populate the quarto.yaml
  ### - copy the lessons from the package materials into the project directory
  ### - create the session_01.qmd etc files in the right spot, linked to the the lessons
  ### maybe a usethis-style "do you want to commit the files?" query
  ###
  ### If the user wishes to create a new structure in an existing course, maybe a flag for
  ### overwriting the existing structure, or a usethis-style "are you sure?"
  ### - delete the existing structure and start over

}
