### get_course_metadata
### verify_course_repo
### setup_quarto_yml
### check_git_steps

### thin wrapper so tests can mock it (base::interactive() is a primitive and
### can't be reliably mocked)
is_interactive <- function() interactive()

check_interactive <- function() {
  ### exported functions are meant to be run by hand in an interactive session,
  ### not from a script.  The option is a bypass for automated tests only.
  if(!is_interactive() && !isTRUE(getOption('lhCore.allow_noninteractive'))) {
    stop("lhCore functions are meant to be run interactively (e.g., from the R console), ",
         "not from a script or non-interactive session.", call. = FALSE)
  }
  return(invisible(TRUE))
}

get_course_metadata <- function() {
  ### get metadata from metadata_course.csv
  if(!file.exists('metadata_course.csv')) {
    stop('No metadata_course.csv detected in the current location: ', getwd())
  }
  metadata_df <- readr::read_csv("metadata_course.csv", show_col_types = FALSE)
  meta <- metadata_df$value
  names(meta) <- metadata_df$field

  return(meta)
}


verify_course_repo <- function(query = 'Is this the correct course location?') {
  here_dir <- here::here()

  query_string <- paste0('Current project = ', here_dir, '... ', trimws(query), ' (y/n) ')
  continue <- readline(query_string)

  if(tolower(continue) != 'y') return(FALSE)

  return(TRUE)
}


setup_quarto_yml <- function(lessons, modules, overwrite = FALSE) {
  quarto_yml_file <- here::here("_quarto.yml")

  ### Check that file exists, and check that SESSION_LINKS are present
  if(!file.exists(quarto_yml_file)) stop("No _quarto.yml file found in current directory...")

  quarto_yml_raw <- readr::read_file(quarto_yml_file)

  if(!overwrite && !grepl("SESSION_LINKS", quarto_yml_raw)) {
    stop("_quarto.yml: No fields for SESSION_LINKS, and overwrite is FALSE - _quarto.yml file not updated")
  }

  ### if overwrite TRUE: reset the old lesson list by replacing everything
  ### between the START and END SESSIONS markers with a fresh SESSION_LINKS
  ### placeholder (keeping the markers and their indentation)
  if(overwrite) {
    pattern <- "(?s)([ \t]*)# START SESSIONS.*?# END SESSIONS"
    if(!grepl(pattern, quarto_yml_raw, perl = TRUE)) {
      stop("_quarto.yml: no '# START SESSIONS' / '# END SESSIONS' markers found - _quarto.yml file not updated")
    }
    quarto_yml_raw <- sub(pattern,
                          "\\1# START SESSIONS\n\\1# SESSION_LINKS\n\\1# END SESSIONS",
                          quarto_yml_raw, perl = TRUE)
  }

  ### define lesson list
  lesson_txt <- define_lesson_txt(lessons, modules)

  ### Update the _quarto.yml with all the good info!
  ### escape backslashes in the replacement so lesson/module names aren't
  ### interpreted as regex backreferences
  quarto_yml_txt <- gsub(" *# SESSION_LINKS", gsub("\\", "\\\\", lesson_txt, fixed = TRUE), quarto_yml_raw)

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, quarto_yml_file)
}

check_git_steps <- function(quiet = FALSE) {
  ### If repo is git and github enabled, return TRUE, otherwise give user message and return FALSE

  ### run git against the course project (not whatever the working directory is)
  course_dir <- here::here()
  git_out <- function(...) {
    suppressWarnings(system2("git", c("-C", shQuote(course_dir), ...), stdout = TRUE, stderr = FALSE))
  }

  is_git <- dir.exists(file.path(course_dir, '.git'))
  if(is_git) {
    is_remote <- any(grepl("^origin", git_out("remote", "-v")))
  } else {
    is_remote <- FALSE
  }

  ### org is only needed for the setup hints below
  if(!is_git || !is_remote) course_org <- get_course_metadata()["course_org"]

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

  ### Check that git config contains user identity - use local values
  ### This should help avoid problem with Windows using different locs for configs!
  git_config <- git_out("config", "--list")
  config_ok  <- git_config[grepl("^user\\.(name|email)=", git_config)]
  has_name   <- any(grepl("^user\\.name=", config_ok))
  has_email  <- any(grepl("^user\\.email=", config_ok))
  if(has_name && has_email) {
    ### success info only; problems below are always reported
    if(!quiet) message("User name and email in Git config: \n  \u2022 ", paste0(config_ok, collapse = "\n  \u2022 "))
  } else {
    message("User name and/or email not detected in Git config!")
    if(.Platform$OS.type == 'windows') {
      message("For Windows operating system, in R console, try:",
              " \n  \u2022 system('git config --global user.name \"my_github_username\"')",
              " \n  \u2022 system('git config --global user.email \"my_email@ucsb.edu\"')")
    } else {
      message("For Mac/Linux, in R console, try:",
              " \n  \u2022 usethis::use_git_config(user.name = \"my_username\", user.email = \"my_email@ucsb.edu\")")
    }
    return(invisible(FALSE))
  }

  return(invisible(TRUE))

}

check_lhlessons_compatibility <- function(pkg = 'lhLessons', lib.loc = NULL) {
  ### Verify that installed lhLessons package is compatible with this version of lhCore.
  ### As of lhCore 0.2.0 and lhLessons 2026.11.10, the folder structure changed:
  ### old: inst/lesson_images/, inst/lesson_data/, inst/lesson_slides/, inst/lesson_resources/
  ### new: inst/lessons/images/, inst/lessons/data/, inst/lessons/slides/, inst/lessons/resources/
  ### lhCore 0.2.0+ requires lhLessons 2026.11.10+
  ### lhCore 0.1.x requires lhLessons 2026.11.08 or earlier

  lhcore_version <- utils::packageVersion('lhCore')
  lhlessons_version <- utils::packageVersion(pkg, lib.loc = lib.loc)

  ### The breaking change: folder restructuring in lhCore 0.2.0 and lhLessons 2026.11.10
  lhcore_new_folder_version <- utils::package_version('0.2.0')
  lhlessons_new_folder_version <- utils::package_version('2026.11.10')

  ### Check: if lhCore is new (0.2.0+), lhLessons must also be new (2026.11.10+)
  if(lhcore_version >= lhcore_new_folder_version &&
     lhlessons_version < lhlessons_new_folder_version) {
    stop("Incompatible package versions detected:\n",
         "  lhCore ", lhcore_version, " (requires lhLessons 2026.11.10 or later)\n",
         "  lhLessons ", lhlessons_version, " (old folder structure)\n",
         "Please upgrade lhLessons to 2026.11.10 or later.",
         call. = FALSE)
  }

  ### Check: if lhCore is old (pre-0.2.0), lhLessons must also be old (pre-2026.11.10)
  if(lhcore_version < lhcore_new_folder_version &&
     lhlessons_version >= lhlessons_new_folder_version) {
    stop("Incompatible package versions detected:\n",
         "  lhCore ", lhcore_version, " (old folder structure, incompatible with new lhLessons)\n",
         "  lhLessons ", lhlessons_version, " (requires lhCore 0.2.0 or later)\n",
         "Please upgrade lhCore to 0.2.0 or later, or downgrade lhLessons to 2026.11.08 or earlier.",
         call. = FALSE)
  }

  return(invisible(TRUE))
}


