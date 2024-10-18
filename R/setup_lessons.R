#' Set up a schedule of course lessons
#'
#' Create the structure necessary to implement an ordered set of lessons in the Quarto book format.
#' The user provides a character vector of lesson names (filenames, bare or with `.qmd` extension);
#' these and associated materials (images, data) are copied from `coreRlessons` package into the
#' course project (created with `init_course()`).  Sets up the index.qmd, _quarto.yml, and some
#' additional files.
#'
#' @param lessons A character vector containing the names (bare or .qmd extension) of the lessons, in order.
#' @param overwrite If there is already an existing lessons structure, and overwrite == FALSE, will return an error.  If the user wishes to create a new lesson structure, then overwrite == FALSE will remove the existing structure and create a new structure based on the lesson names.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{setup_lessons(lessons = c('activity_reproducibility_lego',
#'                                    'lecture_tidy_data.qmd',
#'                                    'github_collaboration',
#'                                    'r_functions.qmd'))}
#' \dontrun{setup_lessons(lessons = available_lessons()$lesson[1:8])}
#' @export
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

  ### make sure coreRlessons package is installed
  installed <- as.data.frame(installed.packages())
  coreRlessons_pkg <- installed[installed$Package == 'coreRlessons', ]
  if(nrow(coreRlessons_pkg) == 0) {
    stop("Please install coreRlessons package: remotes::install_github('nceas-learning-hub/coreRlessons')")
  }

  coreRlessons_vrs <- coreRlessons_pkg$Version
  message('Installing lessons from coreRlessons, version ', coreRlessons_vrs, '...')

  ### convert lessons vec into qmd filenames
  #
  lessons_qmd <- stringr::str_detect(lessons, '\\.qmd$')
  if(any(!lessons_qmd)) {
    message('  Appending .qmd to bare filenames in lessons vector...')
    lessons <- ifelse(lessons_qmd, lessons, paste0(lessons, '.qmd'))
  }

  ### check that all lessons are in coreRlessons
  lessons_available <- available_lessons()$lesson_file
  lessons_missing <- lessons[!lessons %in% basename(lessons_available)]
  if(length(lessons_missing) > 0) stop("Some lessons are not found in the coreRlessons package:",
                                       paste0("\n\u25CF ", lessons_missing))


  ### copy over files from coreRlessons to current project: lessons, images, data
  copy_lessons(from = 'lessons',       to = 'lessons', lessons, directory = FALSE)
  copy_lessons(from = 'lesson_images', to = 'images',   lessons, directory = TRUE)
  copy_lessons(from = 'lesson_data',   to = 'data',    lessons, directory = TRUE)

  ### create session_XX.qmd etc in root (delete old first, in case of high numbers)
  session_fs <- list.files(here::here(), pattern = 'session_.+.qmd', full.names = TRUE)
  if(length(session_fs) > 0 & !overwrite) {
    stop('There are existing session files - if you want to overwrite, set overwrite = TRUE')
  } else {
    unlink(session_fs)
  }

  for(id in 1:length(lessons)) {
    ### id <- 2
    lesson <- lessons[id]
    create_session_file(lesson, id, overwrite)
  }

  ################################
  ### set up additional files! ###
  ################################

  ### create _quarto.yml - including links to sessions, course metadata, etc
  create_quarto_yml(lessons, version = coreRlessons_vrs, overwrite)

  ### create index.qmd - with description from metadata
  create_index_qmd(overwrite)

  ### Other files needed to create the book
  addl_filenames <- c('book.bib', 'cover.png', 'style.css', 'toc.css')
  addl_sys_files <- system.file('course_files', addl_filenames, package = 'coreR')
  file.copy(addl_sys_files, here::here(addl_filenames))

  message('Course populated with lessons!  Refresh the file pane to see the
          course documentation.\n')
  message('To render the book, restart RStudio to activate the Build tab.')
}

### not exported!

copy_lessons <- function(from, to, lessons, directory = FALSE) {
  ### from is the directory to copy lessons from (inside the coreRlessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames;
  ### directory = FALSE for qmds, TRUE for image folder, data folder, etc.
  if(!is.logical(directory)) stop('The directory argument must be TRUE or FALSE - are you copying a directory?')

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder)

  ### copy over folders and files from coreRlessons to current project
  if(directory) {
    fs <- stringr::str_remove(lessons, '.qmd$')
  } else fs <- lessons

  fs_available <- list.files(system.file(from, package = 'coreRlessons'), full.names = TRUE)
  fs_to_copy <- fs_available[basename(fs_available) %in% fs]
  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, subfolder, recursive = directory)
  }
}


create_session_file <- function(lesson, id, overwrite) {
  ### set up a session file for a single lesson: update the include field,
  ### and update the title field by pulling the title from the lesson qmd.
  session_template <- system.file('course_files', 'session_template.qmd', package = 'coreR')
  session_file <- here::here(sprintf('session_%02d.qmd', id))

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
  lesson_text <- readr::read_delim(here::here('lessons', lesson),
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

create_quarto_yml <- function(lessons, version, overwrite = FALSE) {
  quarto_yml_template <- system.file('course_files', '_quarto_template.yml', package = 'coreR')
  quarto_yml_file <- here::here('_quarto.yml')

  if(!overwrite & file.exists(quarto_yml_file)) {
    stop('File exists: ', quarto_yml_file, ' but overwrite is FALSE - _quarto.yml file not updated')
  }

  ### copy a clean version of the template
  file.copy(quarto_yml_template, quarto_yml_file, overwrite = overwrite)

  ### get metadata for fields
  meta <- get_course_metadata()
  course_url <- file.path('https://github.com', meta['course_org'], meta['course_proj'])

  ### define session list
  sessions_vec <- sprintf('session_%02d.qmd', 1:length(lessons))
  lessons_name_vec <- stringr::str_remove(lessons, '.qmd') |>
    stringr::str_replace_all('[^a-z0-9]+', ' ') |>
    stringr::str_replace(' ', ': ')
  ses_les_vec <- sprintf('    - %s  ### %s   (coreRlessons v%s)',
                         sessions_vec, lessons_name_vec, version) |>
    paste(collapse = '\n')

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- readr::read_file(quarto_yml_file) |>
    stringr::str_replace('COURSE_TITLE', meta['title']) |>
    stringr::str_replace('COURSE_DATES', paste(meta['start_date'], '-', meta['end_date'])) |>
    stringr::str_replace('COURSE_URL', course_url) |>
    stringr::str_replace(' *SESSION_LINKS', ses_les_vec)

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, quarto_yml_file)
}

get_course_metadata<- function() {
  ### get metadata from course_metadata.txt
  metadata_txt <- readr::read_file('course_metadata.txt')
  meta_raw <- stringr::str_split(metadata_txt, '\\n') |> unlist()
  meta <- meta_raw |>
    stringr::str_remove('.+= ') |>
    setNames(stringr::str_remove(meta_raw, ' = .+'))

  return(meta)
}

create_index_qmd <- function(overwrite = FALSE) {
  index_template <- system.file('course_files', 'index_template.qmd', package = 'coreR')
  index_file <- here::here('index.qmd')
  if(!overwrite & file.exists(index_file)) {
    stop('File exists: ', index_file, ' but overwrite is FALSE - index.qmd file not updated')
  }
  ### copy a clean version of the template
  file.copy(index_template, index_file, overwrite = overwrite)

  ### get metadata for fields
  meta <- get_course_metadata()

  ### read index and update description
  index_txt <- readr::read_file(index_file) |>
    stringr::str_replace('COURSE_DESCRIPTION', meta['description'])

  ### write out updated index.qmd file
  readr::write_file(index_txt, index_file)
}
