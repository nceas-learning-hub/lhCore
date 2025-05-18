#' Set up a schedule of course lessons
#'
#' Create the structure necessary to implement an ordered set of lessons in the Quarto book format.
#' The user provides a character vector of lesson names (filenames, bare or with `.qmd` extension);
#' these and associated materials (images, data) are copied from `coreRlessons` package into the
#' course project (created with `init_course()`).  Sets up the index.qmd, _quarto.yml, and some
#' additional files.
#'
#' @param lessons Information on the lessons to be included in the course, as a
#'     character vector or data.frame.  A character vector containing the names
#'     (bare or .qmd extension) of the lessons, in order; optionally, a named
#'     character vector where the names are course modules (e.g., "Day 1", "Day 2").
#'     Alternately, a data.frame with columns "module" and "lesson", similar to the
#'     named character vector.
#' @param package The name of the course lessons package to use.
#' @param modules An additional way to provide module information for the lessons:
#'     a vector of module names that matches the length of the lessons vector. Caution:
#'     if `modules` is not NULL, and module names are provided directly in `lessons`,
#'     this argument will take precedence.
#' @param overwrite If there is already an existing lessons structure, and
#'     `overwrite = FALSE`, will return an error.  If the user wishes to create
#'     a new lesson structure, then `overwrite = TRUE` will remove the existing
#'     lesson structure in its entirety and create a new structure
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' setup_lessons(lessons = c("activity_reproducibility_lego",
#'                           "lecture_tidy_data.qmd",
#'                           "github_collaboration",
#'                           "r_functions.qmd"))
#' ### modules as named vector
#' lesson_mods <- available_lessons()$lesson[1:6] |> setNames(c(1, 1, 2, 2, 3, 3))
#' setup_lessons(lessons = lesson_mods)
#'
#' ### modules as argument
#' setup_lessons(lessons = available_lessons()$lesson[1:6],
#'               modules = rep(c("day 1", "day 2"), each = 3))
#'
#' ### modules and course as dataframe
#' example_course <- coreRlessons::example_course
#' head(example_course, 3)
#' #   module                        lesson
#' # 1  Day 1    r_programming_introduction
#' # 2  Day 1    r_quarto_literate_analysis
#' # 3  Day 1 activity_reproducibility_lego
#' setup_lessons(example_course)
#' }
#' @export

setup_lessons <- function(lessons, package = 'lhLessons', modules = NULL, overwrite = FALSE) {

  verify_course_repo(query = 'Set up course lessons here?')

  ### Query lesson version (checks to ensure lessons package is installed!)
  v <- get_lessons_version(pkg = package, quiet = TRUE)
  message("Installing lessons from ", package, ", version ", v, "...")

  ### If lessons provided as data.frame, break into separate lesson and module vectors
  if(any(class(lessons) == "data.frame")) {
    modules <- lessons$module
    lessons <- lessons$lesson
  }
  ### If lessons provided as a named vector, break into separate lesson and module vectors
  ### because names are a fragile attribute...
  if(!is.null(names(lessons))) {
    modules <- names(lessons)
  }
  if(!is.null(modules)) {
    if(length(modules) != length(lessons)) stop('Length of lessons and modules must match!')
  }

  ### strip qmd and rmd extensions from lesson vector
  lessons <- stringr::str_remove(lessons, '\\..md$')


  ### check that all lessons are in coreRlessons
  lessons_available <- available_lessons(pkg = package)$lesson
  lessons_missing <- lessons[!lessons %in% lessons_available]
  if(length(lessons_missing) > 0) stop("Some lessons are not found in the ', package, ' package:",
                                       paste0("\n\u25CF ", lessons_missing))

  ### Set up _quarto.yml with appropriate links to lessons
  if(overwrite) stop('in setup_lessons(), need to add facility to remove old lessons from _quarto.yml and old files before overwriting!')
  ### set up _quarto.yml with links to sessions (other info in setup_course_structure.R)
  setup_quarto_yml(lessons, modules, prefix = lesson_prefix, overwrite)

  ### copy over lesson files from lessons package to current project: lessons, images, data, _extensions
  lesson_prefix <- "s" ### appends sXX_ to start of lesson filenames
  lessons_copied <- copy_lessons(lessons, from = "lessons", to = ".", prefix = lesson_prefix)

  ### copy over lesson-associated folders from lessons package to current project: lessons, images, data
  copy_folders(lessons, from = "lesson_images", to = "images", pkg = package)
  copy_folders(lessons, from = "lesson_data",   to = "data",   pkg = package)
  copy_folders(lessons, from = "lesson_slides", to = "slides", pkg = package)

  ### Set up and write out lessons metadata: module, lesson, lesson files, lesson repo, and repo version
  meta <- data.frame(module = ifelse(exists('modules'), modules, NA),
                     lesson = lessons) |>
    dplyr::left_join(lessons_copied, by = 'lesson') |>
    dplyr::mutate(package = package, pkg_version = v)
  readr::write_csv(meta, here::here('metadata_lessons.csv'))

  message("Course populated with lessons!  Refresh the file pane to see the
          course files and folders.\n")

  message("To render the course book, restart RStudio to activate the Build tab.")

  check_git_steps()

}

### not exported!

copy_lessons <- function(lessons, from, to = ".", prefix, pkg) {
  ### from is the directory to copy lessons from (inside the lessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames, without extensions
  ### prefix is appended to the start of the filename to sort in order (default "s" for session)

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder)

  ### copy over files from lessons package to current project
  fs_avail <- available_lessons(lessons) ### built in error check for missing lessons

  fs_to_copy <- fs_avail$lesson_file[order(match(fs_avail$lesson, lessons))]

  fs_out <- sprintf('%s/%s%02d_%s', subfolder, prefix, 1:length(lessons), basename(fs_to_copy))

  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, fs_out)
  }

  return(data.frame(lesson = lessons,
                    lesson_file_package = basename(fs_to_copy),
                    lesson_file_course  = basename(fs_out)))
}

copy_folders <- function(lessons, from, to, pkg) {
  ### from is the directory to copy lessons from (inside the lessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames;

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder)

  lessons <- sub("\\..md", "", lessons) ### strip extension if necessary

  ### copy over folders and files from lessons package to current project
  fs_available <- list.files(system.file(from, package = pkg), full.names = TRUE)
  fs_to_copy <- fs_available[basename(fs_available) %in% lessons]

  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, subfolder, recursive = TRUE)
  }

}


define_lesson_txt <- function(lessons, modules, prefix) {

  if(is.null(modules)) {
    ### if lessons is not a named vector, simple case
    ### `lessons` vector is bare filenames, no extension - standardize to .qmd
    lesson_txt <- sprintf("      - %s%02d_%s.qmd",
                           prefix, 1:length(lessons), lessons) |>
      paste(collapse = "\n")
  } else {
    message("Module names detected...")
    ### if lessons is a named vector, break into modules by name
    mod_names <- unique(modules) ### keep in order
    mod_vec <- mod_names
    names(mod_vec) <- mod_names

    ### prefix and number the lessons first; attach module names
    lsn_path <- sprintf('%s%02d_%s', prefix, 1:length(lessons), lessons)
    names(lsn_path) <- names(lessons)

    ### loop over modules to create a unique set for each, with part and chapter
    for(mod in mod_names) {
      # mod <- mod_names[2]
      mod_lsns <- lsn_path[modules == mod]
      ### `lessons` vector is bare filenames, no extension - standardize to .qmd
      mod_lsn_vec <- sprintf("        - %s.qmd",
                             mod_lsns) |>
        paste(collapse = "\n")
      mod_txt <- sprintf("      - section: \"%s\"\n        contents:\n%s",
                         mod, mod_lsn_vec)
      mod_vec[mod] <- mod_txt
    }

    lesson_txt <- paste(mod_vec, collapse = "\n")
  }

  return(lesson_txt)
}

