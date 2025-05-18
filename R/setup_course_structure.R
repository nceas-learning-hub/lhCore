#' Set up the basic structure of a course
#'
#' This function follows from init_course() to set up the basic file structure of
#' a new course.  Sets up the _quarto.yml, theme files, and an index.qmd file
#' based on the user's specification of a course template.  These files are pulled
#' from the `lhLessons` package.
#'
#' @param template The name of the course template to use.  This should be a character
#'     length 1 object.  Default template will be `coreR` but other templates may
#'     include `ADC`, `Delta`, `LTER`, etc.  Template affects the appearance of the
#'     course book (scss, logos), and the default index boilerplate.
#' @param package The name of the course lessons package to use.
#' @param overwrite If there is already an existing course structure, and
#'     `overwrite = FALSE`, will return an error.  If the user wishes to create
#'     a new lesson structure, then `overwrite = TRUE` will remove the existing
#'     structure (including lessons) in its entirety and create a new structure from scratch.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#'   setup_course_structure(template = 'coreR')
#' }
#' @export

setup_course_structure <- function(template = c('lh', 'adc', 'delta', 'corer')[1], package = 'lhLessons', overwrite = FALSE) {

  verify_course_repo(query = 'Install course structure here?')

  ### Query lesson version (checks to ensure lessons package is installed!)
  v <- get_lessons_version(pkg = package, quiet = TRUE)
  message(sprintf("Installing structure template from %s, version %s...", package, v))

  ################################
  ###     init _quarto.yml     ###
  ################################

  ### no template-specific info on _quarto.yml, so far... copy from lessons and add
  ### course name and dates from metadata

  init_quarto_yml(package = package, overwrite = overwrite)

  ################################
  ###  setup for gha publish   ###
  ################################

  ### copy GHA files and copy gitignore to ignore docs/

  gitignore_f <- system.file("course_files/gitignore_template", package = package)
  file.copy(gitignore_f, here::here(".gitignore"), overwrite = TRUE)

  gha_fs <- list.files(system.file("course_files/gha_publish", package = package),
                       recursive = TRUE, full.names = TRUE)
  dir.create(here::here(".github")); dir.create(here::here(".github/workflows"))
  file.copy(gha_fs, here::here(".github/workflows"))

  ################################
  ###     set up index.qmd     ###
  ################################
  index_template <- system.file("course_files/index",
                                sprintf("index_%s.qmd", tolower(template)),
                                package = package)

  ### file error checks
  if(!file.exists(index_template)) stop("Template file does not exist: ", index_template)

  index_file <- here::here("index.qmd")
  if(!overwrite & file.exists(index_file)) stop("File exists: ", index_file, " but overwrite is FALSE - index.qmd file not updated")

  ### copy a clean version of the template
  file.copy(index_template, index_file, overwrite = overwrite)

  ################################
  ### Install theme and banner ###
  ################################

  ### for now, just keep the same theme for all regardless of template
  install_theme(org = 'nceas-learning-hub', theme = template)
      ### NOTE: this copies folders from the theme repo, rather than quarto install -
      ### revisit this later!

  ################################
  ###    Install misc files    ###
  ################################

  ### Any other files in the course_files folder should be copied to the root of the course
  misc_fs <- list.files(system.file("course_files", package = package),
                        full.names = TRUE)
  misc_fs <- misc_fs[!dir.exists(misc_fs) & !stringr::str_detect(basename(misc_fs), '_quarto_template.yml|gitignore')]
  file.copy(misc_fs, here::here())


  ################################
  ###  Report out next steps   ###
  ################################

  message("To render the course book, restart RStudio to activate the Build tab.")
  message("Recommended next steps: ",
          "\n  \u2022 Edit the index.qmd to ensure it accurately describes the course",
          "\n  \u2022 Use `lhCore::available_lessons(<lesson package>)` to see the lesson catalog from the lesson package",
          "\n  \u2022 Use `lhCore::setup_lessons(<lessons>)` to install the lessons in the course")
  check_git_steps()

}
