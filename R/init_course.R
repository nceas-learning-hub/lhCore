#' Initialize a lhCore course
#'
#' This will set up the basic project structure for a new course: a project
#' repository, plus a file containing metadata about the course.  The metadata
#' file will be used to help populate details in later functions.
#'
#' @param course_proj The name of the project directory (and Github repo) for the course.
#'     Recommended format: YYYY-MM-<course type>.
#' @param course_org The name of the Github organization to which the course
#'     will belong.
#' @param course_title A brief but descriptive title for the course.  If NULL
#'     (default), the title will be set the same as the project name.
#' @param course_desc A sentence or two describing the course.  If NULL
#'     (default), will be left empty.
#' @param course_dates The dates of the course (as character, so any preferred format).
#'     If NULL, will be left empty.
#' @param loc The file location where the course repository will be created.
#'     Defaults to the current working directory.
#'
#' @return stuff
#' @export
#'
#' @examples
#' \dontrun{
#' init_course(course_proj  = "2024-10-coreR",
#'             course_title = "CoreR October 2024",
#'             course_desc  = "CoreR course offered at NCEAS in Oct 2024",
#'             course_dates = "Oct. 6 - 10, 2024")
#' }
#'

init_course <- function(course_proj,
                        course_org   = 'nceas-learning-hub',
                        course_title = NULL,
                        course_desc  = NULL,
                        course_dates = NULL,
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
  if(is.null(course_desc))  course_desc <- course_title
  if(is.null(course_dates)) course_dates <- '... course dates ...'

  metadata_df <- data.frame(
    field = c('course_proj', 'course_org', 'course_title', 'course_desc', 'course_dates'),
    value = c( course_proj,   course_org,   course_title,   course_desc,   course_dates))
  readr::write_csv(metadata_df, file.path(repo_path, 'metadata_course.csv'))
  # message("Metadata file created at ", normalizePath(repo_path), '/metadata_course.csv')
  # print(metadata_df)

  ### include setup_course_structure here, with arg for template

  ### include git and github initialization here, unless flagged as FALSE

  ### setup gh-pages branch and GHA file here

  message("Recommended next steps: open the new project in RStudio, and then:",
          "\n  \u2022 Edit the `index.qmd` to ensure it accurately describes the course",
          "\n  \u2022 Use `lhCore::setup_course_structure()` to establish the quarto structure and index from a template",
          "\n  \u2022 and then use `lhCore::setup_lessons()` to assemble a chosen set of lessons into a course")

  open_proj <- readline('Do you wish to open the new project in RStudio? (y/n) ')
  if(tolower(open_proj) == 'y') usethis::proj_activate(repo_path)
}
