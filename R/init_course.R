#' Initialize a coreR course
#'
#' @param course_name The name of the course to be created.  This will set up a
#'     new R project with the given name, populate it with some basic structure, and
#'     optionally commit the project as a Github repository.  This will not
#'     create the lesson structure.
#' @param loc The file location where the course repository will be created.
#'     Defaults to the current working directory.
#'
#' @return stuff
#' @export
#'
#' @examples
#' # init_course(course_name = "2024-10-coreR")

init_course <- function(course_name, loc = ".") {
  ### set up a new project using usethis::create_project(path = "MyNewProject", open = TRUE, rstudio = TRUE)
  if(stringr::str_detect(course_name, "[^A-Za-z0-9-_]")) {
    course_name_old <- course_name
    course_name <- stringr::str_replace_all(course_name, "[^A-Za-z0-9-_]+", "_")
    warning("Non-valid characters detected in course name and fixed.  Old name: ",
            course_name_old, " changed to new name: ", course_name)
  }

  repo_path <- file.path(loc, course_name)
  usethis::create_project(path = repo_path, open = TRUE, rstudio = TRUE)

  ### set up desired file structure in the new repository
  ### For static stuff, can we just copy a file structure from the inst/ folder?
  ### and then create the custom stuff with code
  # materials
  #    |------ sections
  #    |
  #    |------ images
  #    |
  #    |------ data
  #    |
  #    |------ style.css
  #    |
  #    |------ toc.css
  #    |
  #    |------ _quarto.yml
  #    |
  #    |------ book.bib
  #    |
  #    |------ cover.png

  ### initialize as Git repo, or provide instructions to the user to do so; same with github.
}
