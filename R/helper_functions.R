### get_course_metadata
### verify course_repo
### init_quarto_yml
### setup_quarto_yml
### check_git_steps

get_course_metadata<- function() {
  ### get metadata from metadata_course.csv
  metadata_df <- readr::read_csv(here::here("metadata_course.csv"))
  meta <- metadata_df$value
  names(meta) <- metadata_df$field

  return(meta)
}


verify_course_repo <- function(query = 'Is this the correct course location?') {
  here_dir <- here::here()

  query_string <- paste0('Current project = ', here_dir, '... ', query, ' (y/n) ')
  continue <- readline(query_string)

  if(tolower(continue) != 'y') {
    stop('Please set the working directory to the course repo and try again.')
  }

  return(NULL)

}

init_quarto_yml <- function(lessons, package, overwrite = FALSE) {

  qmd_yml_f_pkg <- list.files(system.file("course_files", package = package),
                              pattern = '_quarto_template.yml',
                              full.names = TRUE)
  if(length(qmd_yml_f_pkg) == 0) stop("No _quarto_template.yml found in package: ", package)
  qmd_yml_f_lcl <- here::here("_quarto.yml")

  ### copy package file to local file
  file.copy(qmd_yml_f_pkg, qmd_yml_f_lcl, overwrite = overwrite)

  ### get metadata for fields
  meta <- get_course_metadata()

  course_repo <- sprintf('https://github.com/%s/%s', meta["course_org"], meta["course_proj"])
  course_url  <- sprintf('%s.github.io/%s', meta["course_org"], meta["course_proj"])

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- readr::read_file(qmd_yml_f_lcl) |>
    stringr::str_replace_all("COURSE_REPO", course_repo) |>
    stringr::str_replace_all("COURSE_URL", course_url) |>
    stringr::str_replace_all("COURSE_TITLE", meta["course_title"]) |>
    stringr::str_replace_all("COURSE_DATES", meta["course_dates"])

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, qmd_yml_f_lcl)
}

setup_quarto_yml <- function(lessons, modules, prefix, overwrite = FALSE) {
  quarto_yml_file <- here::here("_quarto.yml")

  ### Check that file exists, and check that SESSION_LINKS are present
  if(!file.exists(quarto_yml_file)) stop("No _quarto.yml file found in current directory...")

  quarto_yml_raw <- readr::read_file(quarto_yml_file)

  if(!overwrite & !stringr::str_detect(quarto_yml_raw, "SESSION_LINKS")) {
    stop("_quarto.yml: No fields for SESSION_LINKS, and overwrite is FALSE - _quarto.yml file not updated")
  }

  ### if overwrite TRUE and no SESSION_LINKS:
  ### * replace text between # START_SESSIONS and # END_SESSIONS with # SESSION_LINKS
  ### * remove the corresponding lesson files

  ### define lesson list
  lesson_txt <- define_lesson_txt(lessons, modules, prefix)

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- quarto_yml_raw |>
    stringr::str_replace_all(" *# SESSION_LINKS", lesson_txt)

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, quarto_yml_file)
}

check_git_steps <- function() {
  ### If repo is git and github enabled, return TRUE, otherwise give user message and return FALSE

  is_git <- dir.exists(here::here('.git'))
  if(is_git) {
    is_remote <- any(stringr::str_detect(system("git remote -v", intern = TRUE), "^origin"))
  } else {
    is_remote <- FALSE
  }

  course_org <- get_course_metadata()["course_org"]

  if(!is_git) {
    message("This project does not seem to be a Git repository.  You might want to:",
            "\n  \u2022 Use `usethis::use_git()` to set up your project as a Git-tracked project, and then...",
            "\n  \u2022 Use `usethis::use_github(organisation = '", course_org,
            "')` to connect the project with Github!\n")
    return(invisible(FALSE))
  } else if(!is_remote) {
    message("This Git-tracked project does not seem to be connected to GitHub.  You might want to:",
            "\n  \u2022 Use `usethis::use_github(organisation = '", course_org,
            "')` to connect the project with Github!\n")
    return(invisible(FALSE))
  }
  return(invisible(TRUE))

}

