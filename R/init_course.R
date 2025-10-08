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
#' @param setup_github Set up the new course for Git/GitHub?  Default `TRUE` will
#'     initialize the course repository as a Git-tracked project, and then will
#'     connect it to a GitHub repository named `course_proj` in the organization
#'     defined by `course_org`.
#' @param template The stylistic template to use when creating the course.  Template
#'     defines the default landing/index page, banner colors, hex logo, etc.  Current
#'     options are: `'lh'` (default) for Learning Hub style and landing page; `'adc'`
#'     for Arctic Data Center; `'delta'` for Delta Stewardship Council; and `'corer'`
#'     for coreR style and landing page.
#' @param package The lessons package to be associated with the course.  The lessons
#'     package is the source of the Quarto structure and index page files.  Currently
#'     just the one lessons package; this is a placeholder for potential future
#'     functionality allowing for customized lessons packages for other users.
#' @param loc The file location where the course repository will be created.
#'     Default (`NULL`) will revert to the current working directory (`'.'`) if
#'     no `.Rproj` detected, otherwise will aim for the directory above (`'..'`).
#'     There is a built-in check in `usethis::create_project()` that will protect
#'     further in case you're in nested project territory though.
#' @param quiet Provide progress and diagnostic messages during course initialization?
#'     Default `FALSE`.
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
                        setup_github = TRUE,
                        template = c('lh', 'adc', 'delta', 'corer')[1],
                        package = 'lhLessons',
                        loc = NULL,
                        quiet = TRUE) {
  ### set up a new project using usethis::create_project(path = "MyNewProject", open = TRUE, rstudio = TRUE)
  if(stringr::str_detect(course_proj, "[^A-Za-z0-9-_]")) {
    course_name_old <- course_proj
    course_proj <- stringr::str_replace_all(course_proj, "[^A-Za-z0-9-_]+", "_")
    warning("Non-valid characters detected in course project name and fixed.  Old name: ",
            course_name_old, " changed to new name: ", course_proj)
  }

  if(is.null(loc)) {
    if(any(stringr::str_detect(list.files('.'), '.Rproj$'))) {
      loc <- '..'
    } else {
      loc <- '.'
    }
  }

  ### Create the repo
  repo_path <- file.path(loc, course_proj)
  usethis::create_project(path = repo_path, open = FALSE, rstudio = TRUE)
  if(!quiet) message('R Project created: ', course_proj, ' at ', normalizePath(repo_path))

  protect_current_path <- getwd()

  ### set working directory to be inside the new repo
  if(!quiet) message('Setting working directory to ', repo_path)
  setwd(repo_path)

  ### Add metadata file - overwrite NULLs for title and description
  if(is.null(course_title)) course_title <- course_proj
  if(is.null(course_desc))  course_desc <- course_title
  if(is.null(course_dates)) course_dates <- ''

  metadata_df <- data.frame(
    field = c('course_proj', 'course_org', 'course_title', 'course_desc', 'course_dates'),
    value = c( course_proj,   course_org,   course_title,   course_desc,   course_dates))
  readr::write_csv(metadata_df, 'metadata_course.csv')
  if(!quiet) message("Metadata file created at ", getwd(), '/metadata_course.csv')
  if(!quiet) print(metadata_df)

  setup_course_structure(template = template, package = package,
                         repo = course_proj, quiet = quiet)

  ### include git and github initialization here, unless flagged as FALSE
  if(setup_github) {
    setup_git_github(repo = course_proj, org = course_org, quiet = quiet)
  }
  check_git_steps()


  if(!quiet) {
    open_proj <- readline('Do you wish to open the new project in RStudio? (y/n) ')
  } else {
    open_proj = 'y'  ### in quiet mode, just open the damn repo already
  }
  if(tolower(open_proj) == 'y') usethis::proj_activate(getwd())

  setwd(protect_current_path)
  if(!quiet) message('Resetting current working directory to ', protect_current_path, '...')
  message('The new course has been created as a new project.  Close this project and open or activate the new course project directly.')
}


################################
###     Helper functions     ###
################################

setup_course_structure <- function(template,
                                   package,
                                   repo,
                                   quiet = FALSE) {
  check_repo_path(repo)
  ### Query lesson version (checks to ensure lessons package is installed!)
  v <- get_lessons_version(pkg = package, quiet = TRUE)
  if(!quiet) message(sprintf("Installing structure template from %s, version %s...", package, v))

  ################################
  ###     init _quarto.yml     ###
  ################################

  ### no template-specific info on _quarto.yml, so far... copy from lessons and add
  ### course name and dates from metadata

  init_quarto_yml(package = package, repo = repo, overwrite = FALSE)

  ################################
  ###  setup for gha publish   ###
  ################################

  ### copy GHA files and copy gitignore to ignore docs/

  gitignore_f <- system.file("course_files/gitignore_template", package = package)
  file.copy(gitignore_f, ".gitignore", overwrite = TRUE)

  gha_fs <- list.files(system.file("course_files/gha_publish", package = package),
                       recursive = TRUE, full.names = TRUE)
  dir.create(".github"); dir.create(".github/workflows")
  file.copy(gha_fs, ".github/workflows")

  ################################
  ###     set up index.qmd     ###
  ################################
  index_template <- system.file("course_files/index",
                                sprintf("index_%s.qmd", tolower(template)),
                                package = package)

  ### file error checks
  if(!file.exists(index_template)) stop("Template file does not exist: ", index_template)

  index_file <- "index.qmd"
  if(file.exists(index_file)) {
    stop("File exists: ", index_file, " - index.qmd file not updated")
  } else {
    ### copy a clean version of the template
    file.copy(index_template, index_file)
  }

  ################################
  ### Install theme and banner ###
  ################################
  install_theme(org = 'nceas-learning-hub', theme = template, repo = repo, quiet = quiet)


  ################################
  ###    Install misc files    ###
  ################################

  ### Any other files in the course_files folder should be copied to the root of the course
  misc_fs <- list.files(system.file("course_files", package = package),
                        full.names = TRUE)
  misc_fs <- misc_fs[!dir.exists(misc_fs) & !stringr::str_detect(basename(misc_fs), '_quarto_template.yml|gitignore')]
  file.copy(misc_fs, '.')


  ################################
  ###  Report out next steps   ###
  ################################

  if(!quiet) {
    message("To render the course book, restart RStudio to activate the Build tab.")
    message("Recommended next steps: ",
            "\n  \u2022 Edit the index.qmd to ensure it accurately describes the course",
            "\n  \u2022 Use `search_lessons()` to see the lesson catalog from the lesson package",
            "\n  \u2022 Use `setup_lessons(<lessons>)` to install the lessons in the course")
  }

}

init_quarto_yml <- function(package, repo, overwrite) {

  qmd_yml_f_pkg <- list.files(system.file("course_files", package = package),
                              pattern = '_quarto_template.yml',
                              full.names = TRUE)
  if(length(qmd_yml_f_pkg) == 0) stop("No _quarto_template.yml found in package: ", package)
  qmd_yml_f_lcl <- "_quarto.yml"

  ### copy package file to local file
  if(file.exists(qmd_yml_f_lcl) & !overwrite) {
    stop ('_quarto.yml file already exists!  Delete and run again')
  } else {
    file.copy(qmd_yml_f_pkg, qmd_yml_f_lcl, overwrite = overwrite)
  }

  ### get metadata for fields
  meta <- get_course_metadata()

  course_repo <- sprintf('https://github.com/%s/%s', meta["course_org"], meta["course_proj"])
  course_url  <- sprintf('%s.github.io/%s', meta["course_org"], meta["course_proj"])
  if(is.na(meta["course_dates"])) {
    ### empty date field; title is all
    course_title = meta["course_title"]
  } else {
    course_title = sprintf("%s (%s)", meta["course_title"], meta["course_dates"])
  }

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- readr::read_file(qmd_yml_f_lcl) |>
    stringr::str_replace_all("COURSE_REPO", course_repo) |>
    stringr::str_replace_all("COURSE_URL", course_url) |>
    stringr::str_replace_all("COURSE_TITLE", course_title)

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, qmd_yml_f_lcl)
}

install_theme <- function(org = 'nceas-learning-hub',
                          theme,
                          repo,
                          quiet = FALSE) {
  check_repo_path(repo)

  ### where is the extension coming from? organisation and repo
  extension_dir <- sprintf('%s/theme_%s', org, theme)

  quarto_add <- sprintf('quarto add %s --no-prompt', extension_dir)

  quarto_msg <- utils::capture.output({
    system(quarto_add)
  }, type = 'message')
  if(!quiet) print(quarto_msg)

  return(extension_dir)
}

setup_git_github <- function(repo, org, quiet) {

  check_repo_path(repo)

  ### git init/add/commit manually to avoid the annoying usethis::use_git() prompt
  x <- gert::git_init()
  x <- gert::git_add(files = '.')
  if(!quiet) print(x)
  x <- gert::git_commit_all(message = 'Initial commit')
  if(!quiet) print(x)

  usethis::use_github(organisation = org)

}

check_repo_path <- function(repo) {
  x <- getwd()
  if(basename(x) != repo) stop('Mismatch between current working directory and repo name!')
  fs <- list.files(x)
  if(!any(stringr::str_detect(fs, '.Rproj$'))) stop('Current working directory does not contain .Rproj!')
  return('all good')
}
