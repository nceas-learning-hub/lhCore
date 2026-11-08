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
#'     (default), will be set the same as the course title.
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
#'     Default (`NULL`) is the current working directory (`'.'`).  The location
#'     must not be inside an existing Git repository (this is checked before
#'     anything is created); if it is, an error is returned and a different
#'     `loc` must be chosen.
#' @param quiet Provide progress and diagnostic messages during course initialization?
#'     Default `TRUE`.
#'
#' @return (invisibly) the full path to the new course project directory
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
                        template = c('lh', 'adc', 'delta', 'corer'),
                        package = 'lhLessons',
                        loc = NULL,
                        quiet = TRUE) {
  ### validate template up front (first option, 'lh', is the default)
  template <- match.arg(template)

  ### check up front, before anything is created, that the course won't land
  ### inside an existing Git repo: the new course gets its own repo, and a
  ### repo nested in another causes trouble later. `loc` may not exist yet, so
  ### test from its nearest existing parent folder.
  if(is.null(loc)) loc <- '.'
  check_dir <- normalizePath(loc, winslash = '/', mustWork = FALSE)
  while(!dir.exists(check_dir) && dirname(check_dir) != check_dir) {
    check_dir <- dirname(check_dir)
  }
  enclosing_repo <- tryCatch(gert::git_find(check_dir), error = function(e) NULL)
  if(!is.null(enclosing_repo)) {
    stop("Cannot create the course in '", loc, "': it is inside the Git repository at ",
         enclosing_repo, ".\n  Use `loc` to choose a location that is not Git-tracked.")
  }

  ### set up a new project using usethis::create_project(path = "MyNewProject", open = TRUE, rstudio = TRUE)
  if(grepl("[^A-Za-z0-9_-]", course_proj)) {
    course_name_old <- course_proj
    course_proj <- gsub("[^A-Za-z0-9_-]+", "_", course_proj)
    warning("Non-valid characters detected in course project name and fixed.  Old name: ",
            course_name_old, " changed to new name: ", course_proj)
  }

  ### usethis honors this option to silence its messages (restored on exit)
  old_opts <- options(usethis.quiet = quiet)
  on.exit(options(old_opts), add = TRUE)

  ### Create the repo
  repo_path <- file.path(loc, course_proj)
  usethis::create_project(path = repo_path, open = FALSE, rstudio = TRUE)
  if(!quiet) message('R Project created: ', course_proj, ' at ', normalizePath(repo_path))

  ### set working directory to be inside the new repo
  if(!quiet) message('Setting working directory to ', repo_path)
  old_wd <- setwd(repo_path)
  on.exit(setwd(old_wd), add = TRUE)
  repo_full_path <- normalizePath(getwd())

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
    publish_gha(repo = course_proj, pkg = package, quiet = quiet)
  }
  check_git_steps(quiet = quiet)


  if(!quiet) {
    open_proj <- readline('Do you wish to open the new project in RStudio? (y/n) ')
  } else {
    open_proj <- 'y'  ### in quiet mode, just open the repo without asking
  }
  if(tolower(open_proj) == 'y') usethis::proj_activate(getwd())

  if(!quiet) message('Resetting current working directory to ', old_wd, '...')
  message('The new course has been created as a new project.  Close this project and open or activate the new course project directly.')

  return(invisible(repo_full_path))
}


################################
###     Helper functions     ###
################################

setup_course_structure <- function(template,
                                   package,
                                   repo,
                                   quiet = FALSE) {
  check_repo_path(repo)

  ### fail early, before anything is created, if index.qmd already exists
  index_file <- "index.qmd"
  if(file.exists(index_file)) {
    stop("File exists: ", index_file, " - index.qmd file not updated")
  }

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
  ###     set up index.qmd     ###
  ################################
  index_template <- system.file("course_files/index",
                                sprintf("index_%s.qmd", tolower(template)),
                                package = package)

  ### file error checks
  if(!file.exists(index_template)) stop("Template file does not exist: ", index_template)

  ### copy a clean version of the template
  if(!file.copy(index_template, index_file)) stop("Failed to copy ", index_template, " to ", index_file)

  ################################
  ### Install theme and banner ###
  ################################
  install_theme(theme_org = 'nceas-learning-hub', 
                theme = template, 
                repo  = repo, 
                quiet = quiet)


  ################################
  ###    Install misc files    ###
  ################################

  ### Any other files in the course_files folder should be copied to the root of the course
  misc_fs <- list.files(system.file("course_files", package = package),
                        full.names = TRUE)
  misc_fs <- misc_fs[!dir.exists(misc_fs) & !grepl('_quarto_template\\\\.yml|gitignore', basename(misc_fs))]
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
  if(file.exists(qmd_yml_f_lcl) && !overwrite) {
    stop ('_quarto.yml file already exists!  Delete and run again')
  } else if(!file.copy(qmd_yml_f_pkg, qmd_yml_f_lcl, overwrite = overwrite)) {
    stop('Failed to copy _quarto_template.yml from ', package)
  }

  ### get metadata for fields
  meta <- get_course_metadata()

  course_repo <- sprintf('https://github.com/%s/%s', meta["course_org"], meta["course_proj"])
  course_url  <- sprintf('%s.github.io/%s', meta["course_org"], meta["course_proj"])
  if(is.na(meta["course_dates"])) {
    ### empty date field; title is all
    course_title <- meta["course_title"]
  } else {
    course_title <- sprintf("%s (%s)", meta["course_title"], meta["course_dates"])
  }

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- readr::read_file(qmd_yml_f_lcl)
  quarto_yml_txt <- gsub("COURSE_REPO",  course_repo,  quarto_yml_txt, fixed = TRUE)
  quarto_yml_txt <- gsub("COURSE_URL",   course_url,   quarto_yml_txt, fixed = TRUE)
  quarto_yml_txt <- gsub("COURSE_TITLE", course_title, quarto_yml_txt, fixed = TRUE)

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, qmd_yml_f_lcl)
}

install_theme <- function(theme_org = 'nceas-learning-hub',
                          theme,
                          repo,
                          quiet = FALSE) {
  ### Currently all themes are hosted at nceas-learning-hub.  The
  ### theme_org can be different from the org (where the course 
  ### repo goes)
  check_repo_path(repo)

  if(!nzchar(Sys.which('quarto'))) stop('quarto CLI not found on the PATH; install Quarto first')

  ### where is the extension coming from? organisation and repo
  extension_dir <- sprintf('%s/theme_%s', theme_org, theme)

  ### quarto writes to stdout/stderr of the child process, which capture.output()
  ### can't catch - so silence it at the system2() call instead (FALSE discards)
  quarto_out <- if(quiet) FALSE else ""
  status <- system2('quarto', c('add', extension_dir, '--no-prompt'),
                    stdout = quarto_out, stderr = quarto_out)
  if(status != 0) warning('quarto add ', extension_dir, ' failed (exit status ', status, ')')

  ### also install fontawesome
  status <- system2('quarto', c('add', 'quarto-ext/fontawesome', '--no-prompt'),
                    stdout = quarto_out, stderr = quarto_out)
  if(status != 0) warning('quarto add quarto-ext/fontawesome failed (exit status ', status, ')')

  return(extension_dir)
}

setup_git_github <- function(repo, org, quiet) {

  check_repo_path(repo)

  ### usethis honors this option to silence its messages
  old_opts <- options(usethis.quiet = quiet)
  on.exit(options(old_opts), add = TRUE)

  ### git init/add/commit manually to avoid the annoying usethis::use_git() prompt
  gert::git_init()

  ### copy whatever identity gert can see into the repo-level config, so that
  ### system git (used internally by quarto publish) sees it too
  cfg   <- gert::git_config_global()
  name  <- cfg$value[cfg$name == "user.name"][1]
  email <- cfg$value[cfg$name == "user.email"][1]
  if(is.na(name) || is.na(email)) {
    stop("No user.name/user.email found. Set them with gert::git_config_global_set()")
  }
  repo_dir <- getwd()
  gert::git_config_set("user.name",  name,  repo = repo_dir)
  gert::git_config_set("user.email", email, repo = repo_dir)

  ### verify that *system* git (used by quarto publish) now sees the identity;
  ### on Windows, R's HOME (OneDrive/Documents) differs from the user profile
  ### that gert/libgit2 reads, so gert can see an identity that system git can't
  sys_email <- suppressWarnings(system2("git", c("config", "user.email"), stdout = TRUE, stderr = FALSE))
  if(length(sys_email) == 0 || !nzchar(sys_email)) {
    stop("System git cannot see user.email even after setting repo-level config in ", repo_dir)
  }


  x <- gert::git_add(files = '.')
  if(!quiet) print(x)
  x <- gert::git_commit_all(message = 'Initial commit')

  b <- gert::git_branch_list()
  if(!'main' %in% b$name && 'master' %in% b$name) {
    ### NOTE: this will only work if the default branch is currently 'master';
    ### if the default branch is anything else, it won't change and there will
    ### be problems later
    if(!quiet) message('Renaming default branch for course from master to main...')
    gert::git_branch_move(branch = 'master', new_branch = 'main')
  }
  usethis::use_github(organisation = org)

}

publish_gha <- function(repo, pkg, quiet = TRUE) {
  ### confirm we're in the course repo before writing any files
  check_repo_path(repo)

  ### copy GHA files and copy gitignore to ignore docs/
  gitignore_f <- system.file("course_files/gitignore_template", package = pkg)
  if(!file.copy(gitignore_f, ".gitignore", overwrite = TRUE)) stop("Failed to copy .gitignore template")

  gha_fs <- list.files(system.file("course_files/gha_publish", package = pkg),
                       recursive = TRUE, full.names = TRUE)
  dir.create(".github/workflows", recursive = TRUE, showWarnings = FALSE)
  file.copy(gha_fs, ".github/workflows", overwrite = TRUE)

  ### instructions here https://quarto.org/docs/publishing/github-pages.html#source-branch
  ### set up gh-pages at origin so the quarto publish command will work without the prompt
  gert::git_branch_create("gh-pages", checkout = FALSE)
  gert::git_push(remote = "origin", refspec = "refs/heads/gh-pages", verbose = !quiet)

  ### NOTE: this line requires default to be main; if default is something
  ### else, this will fail - see check in setup_git_github() about renaming master branch to main
  gert::git_branch_checkout("main")
  ### git/quarto chatter goes to stderr; ignored when quiet (a failure still
  ### shows up as a non-zero exit status, which we turn into a warning)
  quarto_out <- if(quiet) FALSE else ""
  status <- system2('quarto', c('publish', 'gh-pages', '--no-prompt'),
                    stdout = quarto_out, stderr = quarto_out)
  if(status != 0) warning("quarto publish gh-pages failed (exit status ", status,
                          "); re-run with quiet = FALSE to see details")
  return(invisible(NULL))
}

check_repo_path <- function(repo) {
  x <- getwd()
  if(basename(x) != repo) stop('Mismatch between current working directory and repo name!')
  fs <- list.files(x)
  if(!any(grepl('\\\\.Rproj$', fs))) stop('Current working directory does not contain .Rproj!')
  return(invisible(TRUE))
}
