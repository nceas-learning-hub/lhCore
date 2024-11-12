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
#' @param modules An additional way to provide module information for the lessons:
#'     a vector of module names that matches the length of the lessons vector. Caution:
#'     if `modules` is not NULL, and module names are provided directly in `lessons`,
#'     this argument will take precedence.
#' @param overwrite If there is already an existing lessons structure, and
#'     `overwrite = FALSE`, will return an error.  If the user wishes to create
#'     a new lesson structure, then `overwrite = TRUE` will remove the existing
#'     structure in its entirety and create a new structure based on the lesson names.
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

setup_lessons <- function(lessons, modules = NULL, overwrite = FALSE) {

  ### Query lesson version (checks to ensure lessons package is installed!)
  v <- get_lessons_version(quiet = TRUE)
  message("Installing lessons from coreRlessons, version ", v, "...")

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
  lessons_available <- available_lessons()$lesson
  lessons_missing <- lessons[!lessons %in% lessons_available]
  if(length(lessons_missing) > 0) stop("Some lessons are not found in the coreRlessons package:",
                                       paste0("\n\u25CF ", lessons_missing))


  ### copy over files from coreRlessons to current project: lessons, images, data
  lesson_prefix <- "s" ### appends sXX_ to start of lesson filenames
  copy_lessons(lessons, from = "lessons", to = ".", prefix = lesson_prefix)
  copy_folders(lessons, from = "lesson_images", to = "images")
  copy_folders(lessons, from = "lesson_data",   to = "data")

  ### create _quarto.yml - including links to sessions, course metadata, etc
  create_quarto_yml(lessons, modules, prefix = lesson_prefix, overwrite)

  ################################
  ### set up additional files! ###
  ################################

  ### create index.qmd - with description from metadata
  create_index_qmd(overwrite)

  ### Other files needed to create the book
  ### possibly just copy everything except those with _template in the name?
  addl_sys_files <- list.files(system.file("course_files", package = "coreRlessons"))
  addl_sys_files <- addl_sys_files[!stringr::str_detect(addl_sys_files, 'template')]
  file.copy(addl_sys_files, here::here(basename(addl_filenames)))

  message("Course populated with lessons!  Refresh the file pane to see the
          course files and folders.\n")

  message("To render the course book, restart RStudio to activate the Build tab.")
  message("To set up Git/GitHub for this course:",
          "\n  \u25CF Use `usethis::use_git()` to set up your project as a Git-tracked project, and then...",
          "\n  \u25CF Use `usethis::use_github(organisation = 'nceas-learning-hub')` to connect the project with Github!\n")

}

### not exported!

copy_lessons <- function(lessons, from, to = ".", prefix = "s") {
  ### from is the directory to copy lessons from (inside the coreRlessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames, without extensions
  ### prefix is appended to the start of the filename to sort in order (default "s" for session)

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder)

  ### copy over files from coreRlessons to current project
  fs_avail <- available_lessons(lessons) ### built in error check for missing lessons

  fs_to_copy <- fs_avail$lesson_file[order(match(fs_avail$lesson, lessons))]

  fs_out <- sprintf('%s/%s%02d_%s', subfolder, prefix, 1:length(lessons), basename(fs_to_copy))

  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, fs_out)
  }
}

copy_folders <- function(lessons, from, to) {
  ### from is the directory to copy lessons from (inside the coreRlessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames;

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder)

  ### copy over folders and files from coreRlessons to current project
  fs_available <- list.files(system.file(from, package = "coreRlessons"), full.names = TRUE)
  fs_to_copy <- fs_available[basename(fs_available) %in% fs]

  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, subfolder, recursive = TRUE)
  }

}


create_quarto_yml <- function(lessons, modules, prefix = "s", overwrite = FALSE) {
  quarto_yml_file <- here::here("_quarto.yml")

  if(!overwrite & file.exists(quarto_yml_file)) {
    stop("File exists: ", quarto_yml_file, " but overwrite is FALSE - _quarto.yml file not updated")
  }

  ### copy a clean version of the template
  quarto_yml_template <- system.file("course_files", "_quarto_template.yml", package = "coreRlessons")
  file.copy(quarto_yml_template, quarto_yml_file, overwrite = overwrite)

  ### get metadata for fields
  meta <- get_course_metadata()
  course_url <- file.path("https://github.com", meta["course_org"], meta["course_proj"])

  ### define lesson list
  lesson_txt <- define_lesson_txt(lessons, modules, prefix)

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- readr::read_file(quarto_yml_file) |>
    stringr::str_replace("COURSE_TITLE", meta["title"]) |>
    stringr::str_replace("COURSE_DATES", paste(meta["start_date"], "-", meta["end_date"])) |>
    stringr::str_replace("COURSE_URL", course_url) |>
    stringr::str_replace(" *SESSION_LINKS", lesson_txt)

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, quarto_yml_file)
}

define_lesson_txt <- function(lessons, modules, prefix) {
  v <- get_lessons_version()

  if(any(!str_detect(lessons, '\\..md$'))) {
    ### attach file extensions
    fs_avail <- available_lessons()
  }

  if(is.null(modules)) {
    ### if lessons is not a named vector, simple case
    lesson_txt <- sprintf("    - %s%02d_%s  ###  (coreRlessons v%s)",
                           prefix, 1:length(lessons), lessons, v) |>
      paste(collapse = "\n")
  } else {
    message("Module names detected...")
    ### if lessons is a named vector, break into modules by name
    mod_names <- unique(modules) ### keep in order
    mod_vec <- mod_names |> setNames(mod_names)

    ### prefix and number the lessons first; attach module names
    lsn_path <- sprintf('%s%02d_%s', prefix, 1:length(lessons), lessons) |>
      setNames(names(lessons))

    ### loop over modules to create a unique set for each, with part and chapter
    for(mod in mod_names) {
      # mod <- mod_names[2]
      mod_lsns <- lsn_path[modules == mod]
      mod_lsn_vec <- sprintf("      - %s  ###  (coreRlessons v%s)",
                             mod_lsns, v) |>
        paste(collapse = "\n")
      mod_txt <- sprintf("    - part: \"%s\"\n      chapters:\n%s",
                         mod, mod_lsn_vec)
      mod_vec[mod] <- mod_txt
    }

    lesson_txt <- paste(mod_vec, collapse = "\n")
  }

  return(lesson_txt)
}

get_course_metadata<- function() {
  ### get metadata from course_metadata.txt
  metadata_txt <- readr::read_file("course_metadata.txt")
  meta_raw <- stringr::str_split(metadata_txt, "\\n") |> unlist()
  meta <- meta_raw |>
    stringr::str_remove(".+= ") |>
    setNames(stringr::str_remove(meta_raw, " = .+"))

  return(meta)
}

create_index_qmd <- function(overwrite = FALSE) {
  index_template <- system.file("course_files", "index_template.qmd", package = "coreRlessons")
  index_file <- here::here("index.qmd")
  if(!overwrite & file.exists(index_file)) {
    stop("File exists: ", index_file, " but overwrite is FALSE - index.qmd file not updated")
  }
  ### copy a clean version of the template
  file.copy(index_template, index_file, overwrite = overwrite)

  ### get metadata for fields
  meta <- get_course_metadata()

  ### read index and update description
  index_txt <- readr::read_file(index_file) |>
    stringr::str_replace("COURSE_DESCRIPTION", meta["description"])

  ### write out updated index.qmd file
  readr::write_file(index_txt, index_file)
}
