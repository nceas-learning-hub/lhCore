#' Initialize a coreR course
#'
#' This will set up the basic project structure for a new course: a project
#' repository, plus a file containing metadata about the course.  The metadata
#' file will be used to help populate details in later functions.
#'
#' @param course_proj The name of the project directory (and Github repo) for the course.
#'     Recommended format: YYYY-MM-coreR.
#' @param course_org The name of the Github organization to which the course
#'     will belong.
#' @param course_title A brief but descriptive title for the course.  If NULL
#'     (default), the title will be set the same as the project name.
#' @param course_description A sentence or two describing the course.  If NULL
#'     (default), will be left empty.
#' @param start_date The starting date (as character, so any preferred format).
#'     If NULL, will be left empty.
#' @param end_date The ending date (use same format as start_date).
#'     If NULL, and start_date is NULL, will be left empty.
#' @param loc The file location where the course repository will be created.
#'     Defaults to the current working directory.
#'
#' @return stuff
#' @export
#'
#' @examples
#' \dontrun{
#' init_course(course_proj = "2024-10-coreR",
#'             course_title = "CoreR October 2024",
#'             course_description = "CoreR course offered at NCEAS in Oct 2024",
#'             start_date = "Oct. 6, 2024",
#'             end_date = "Oct. 10, 2024")
#' }
#'

init_course <- function(course_proj,
                        course_org = 'nceas-learning-hub',
                        course_title = NULL,
                        course_description = NULL,
                        start_date = NULL,
                        end_date = NULL,
                        loc = ".") {
  ### set up a new project using usethis::create_project(path = "MyNewProject", open = TRUE, rstudio = TRUE)
  if(stringr::str_detect(course_proj, "[^A-Za-z0-9-_]")) {
    course_name_old <- course_proj
    course_proj <- stringr::str_replace_all(course_proj, "[^A-Za-z0-9-_]+", "_")
    warning("Non-valid characters detected in course project name and fixed.  Old name: ",
            course_name_old, " changed to new name: ", course_proj)
  }

  ### Create the repo
  repo_path <- file.path(loc, course_proj)
  usethis::create_project(path = repo_path, open = FALSE, rstudio = TRUE)
  message("R Project created: ", course_proj, " at ", normalizePath(repo_path))

  ### Add metadata file - overwrite NULLs for title and description
  if(is.null(course_title)) course_title <- course_proj
  if(is.null(course_description)) course_description <- course_title
  if(is.null(start_date)) start_date <- '2024-01-01'
  if(is.null(end_date)) end_date <- '2024-12-31'

  metadata_txt <- sprintf('course_proj = %s\ncourse_org = %s\ntitle = %s\ndescription = %s\nstart_date = %s\nend_date = %s',
                          course_proj, course_org, course_title, course_description, start_date, end_date)
  readr::write_file(metadata_txt, file.path(repo_path, 'course_metadata.txt'))
  message("Metadata file created at ", normalizePath(repo_path), '/course_metadata.txt')
  cat(metadata_txt, '\n\n')

  message('Recommended next steps: open the new project in RStudio, and then:',
          "\n  \u25CF Use `coreR::available_lessons()` to see the lesson catalog from coreRlessons",
          "\n  \u25CF Use `coreR::setup_lessons(<lessons>)` to install the lessons in the course",
          "\n  \u25CF Use `usethis::use_git()` to set up your project as a Git-tracked project, and then...",
          "\n  \u25CF Use `usethis::use_github(organisation = '", course_org,
          "')` to connect the project with Github!\n")
  open_proj <- readline('Do you wish to open the new project in RStudio? (y/n)')
  if(tolower(open_proj) == 'y') usethis::proj_activate(repo_path)
}
