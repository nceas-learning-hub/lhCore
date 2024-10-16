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

  ### Check that you're in a legit coreR course directory!  how to check for this?
  ### * dummy file?
  ### * something in the description or README.md?
  ### * get the root directory of the coreR course - here::here()?

  ### make sure coreRlessons package is installed (not necessarily attached?)
  installed <- as.data.frame(installed.packages())
  coreRlessons_pkg <- installed[installed$Package == 'coreRlessons', ]
  if(nrow(coreRlessons_pkg) == 0) stop("Please install coreRlessons package: remotes::install_github('nceas-learning-hub/coreRlessons')")
  coreRlessons_vrs <- coreRlessons_pkg$Version
  message('Installing lessons from coreRlessons, version ', coreRlessons_vrs, '...')

  ### convert lessons vec into qmd filenames
  # lessons <- c('a', 'b.qmd', 'git-github-intro', 'r-creating-functions.qmd')
  lessons_qmd <- stringr::str_detect(lessons, '\\.qmd$')
  if(any(!lessons_qmd)) {
    message('  Appending .qmd to bare filenames in lessons vector...')
    lessons <- ifelse(lessons_qmd, lessons, paste0(lessons, '.qmd'))
  }

  ### check that all lessons are in coreRlessons
  lessons_available <- list.files(system.file('sections', package = 'coreRlessons'), full.names = TRUE)
  lessons_missing <- lessons_clean[!lessons_clean %in% basename(lessons_available)]
  if(length(lessons_missing) > 0) stop("Some lessons are not found in the coreRlessons package:",
                                       paste0("\n\u25CF ", lessons_missing))

  ### create materials/sections directory
  if(!dir.exists("materials")) dir.create("materials")
  if(!dir.exists("materials/sections")) dir.create("materials/sections")

  ### copy over sections from coreRlessons to current project
  lessons_to_copy <- lessons_available[basename(lessons_available) %in% lessons]
  file.copy(lessons_to_copy, 'materials/sections')

  ### create session_01.qmd etc in materials (delete old first, in case of high numbers)
  ### * copy a template from this package
  ### * populate templates accordingly with the correct filenames
  ### create quarto.yaml
  ###

}
