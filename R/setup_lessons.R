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
  # lessons <- c('activity_reproducibility_lego', 'lecture_tidy_data.qmd', 'github_collaboration', 'r_functions.qmd')
  lessons_qmd <- stringr::str_detect(lessons, '\\.qmd$')
  if(any(!lessons_qmd)) {
    message('  Appending .qmd to bare filenames in lessons vector...')
    lessons <- ifelse(lessons_qmd, lessons, paste0(lessons, '.qmd'))
  }

  ### check that all lessons are in coreRlessons
  lessons_available <- list.files(system.file('lessons', package = 'coreRlessons'), full.names = TRUE)
  lessons_missing <- lessons[!lessons %in% basename(lessons_available)]
  if(length(lessons_missing) > 0) stop("Some lessons are not found in the coreRlessons package:",
                                       paste0("\n\u25CF ", lessons_missing))


  ### copy over files from coreRlessons to current project: lessons, images, data
  copy_lessons(dir_name = 'lessons', lessons, directory = FALSE)
  copy_lessons(dir_name = 'images', lessons, directory = TRUE)
  copy_lessons(dir_name = 'data', lessons, directory = TRUE)

  ### create session_XX.qmd etc in materials (delete old first, in case of high numbers)
  session_fs <- list.files('materials', pattern = 'session_.qmd', full.names = TRUE)
  if(length(session_fs) > 0 | !overwrite) {
    stop('There are existing session files - if you want to overwrite, set overwrite = TRUE')
  }
  unlink(session_fs)

  for(id in 1:length(lessons)) {
    ### id <- 2
    lesson <- lessons[id]
    create_session_file(lesson, id, overwrite)
  }

}

### not exported!

copy_lessons <- function(dir_name, lessons, directory = FALSE) {
  ### dir_name is the directory to copy lessons from;
  ### lessons is the list of lesson filenames;
  ### directory = FALSE for qmds, TRUE for image folder, data folder, etc.
  if(!is.logical(directory)) stop('The directory argument must be TRUE or FALSE - are you copying a directory?')

  ### create materials directory and subdir as needed
  if(!dir.exists("materials")) dir.create("materials")
  subfolder <- file.path("materials", dir_name)
  if(!dir.exists(subfolder)) dir.create(subfolder)

  ### copy over folders and files from coreRlessons to current project
  if(directory) {
    fs <- stringr::str_remove(lessons, '.qmd$')
  } else fs <- lessons

  fs_available <- list.files(system.file(dir_name, package = 'coreRlessons'), full.names = TRUE)
  fs_to_copy <- fs_available[basename(fs_available) %in% fs]
  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, subfolder, recursive = directory)
  }
}


create_session_file <- function(lesson, id, overwrite) {
  ### set up a session file for a single lesson: update the include field,
  ### and update the title field by pulling the title from the lesson qmd.
  session_template <- system.file('course_files', 'session_template.qmd', package = 'coreR')
  session_file <- file.path('materials', sprintf('session_%02d.qmd', id))

  if(!overwrite & file.exists(session_file)) {
    warning('File exists: ', session_file, ' but overwrite is FALSE - session file not updated')
  }

  if(!file.exists(session_file) | overwrite) {
    file.copy(session_template, session_file)
    update_session_include(lesson, session_file)
    update_session_title(lesson, session_file)
  }

  return(session_file)
}

update_session_include <- function(lesson, session_file) {
  session_txt <- readr::read_file(session_file)
  include_path <- file.path('/lessons', lesson)
  session_txt_out <- stringr::str_replace(session_txt, 'LESSON FILE', include_path)
  return(readr::write_file(session_txt_out, session_file))
}

update_session_title <- function(lesson, session_file) {
  ### WHERE DO THE TITLES COME FROM????!!!???
  ### * Idea: in the lesson yaml header, include a `lesson_title` field
  ###    * since `lesson_title` is not recognized by Quarto it looks like it is ignored;
  ###    * perhaps we can include other metadata up there too, like author, date, etc...
  lesson_text <- readr::read_delim(file.path('materials/lessons', lesson),
                                   delim = '\\n', col_names = FALSE, show_col_types = FALSE)
  lesson_title <- lesson_text$X1[stringr::str_detect(lesson_text$X1, 'lesson_title')] |>
    stringr::str_remove('.+:') |>
    stringr::str_squish()

  if(length(lesson_title) == 1) {
    session_txt <- readr::read_file(session_file)
    session_txt_out <- stringr::str_replace(session_txt, 'LESSON TITLE', lesson_title)
    return(readr::write_file(session_txt_out, session_file))
  } else {
    warning('Lesson title not found for lesson ', lesson)
    return(FALSE)
  }
}
