#' Update lessons in a lhCore course
#'
#' @param lessons A vector of specific lessons to be updated, based on file
#'     names (bare or with `.qmd` extension).  If lesson names don't match
#'     lessons in the source book, an error will be returned.  The default `all`
#'     will update all available lessons, i.e., those whose lesson names match
#'     those in the source book.  Customized lessons, whose names do not match
#'     those in the source book, will not be updated, even with `version = 'all'`.
#' @param pkg Name of the package where lessons are curated, default "lhLessons"
#' @param version A version of the lhCore package from which the updated lessons
#'     should be pulled.  `version` can be given as a commit hash, a tag, a
#'     version, or a date.
#' @param org The GitHub organization where the lessons package repo lives.
#'     Default "nceas-learning-hub"
#'
#' @return NULL (invisibly)
#' @export
#'
#' @examples
#' # update_lessons(lessons = c("git-github-intro", "r-creating-packages.qmd"),
#' #                version = "v0.0.1")

update_lessons <- function(lessons = "all", pkg = "lhLessons", version = NULL,
                           org = "nceas-learning-hub") {
  ### placeholder - give a vector of lesson names, optionally give a version tag
  ### if version tag is NULL use latest otherwise use whatever the user supplied
  ### in the `@ref` portion - could be a commit, tag, branch name
  ### install_github("nceas-learning-hub", "lhLessons@v0.1.1")

  ### if version is not current, install lhLessons from requested version
  ### identify lessons to be updated - which match the names in lhLessons at this version
  ### copy over into sections folder, overwriting old versions

  installed_version <- get_lessons_version(pkg, quiet = TRUE)
  if(!is.null(version) && installed_version == version) {
    stop("Requested version (", version, ") is identical to installed version (", installed_version, ")...")
  }

  #####################################################
  ### Set up temporary package with desired version ###
  #####################################################

  ### Set up temp library location for desired version
  tmp_lib <- tempfile()
  dir.create(tmp_lib)

  ### clean up the temp library on any exit, including errors and cancellation
  on.exit(unlink(tmp_lib, recursive = TRUE), add = TRUE)

  ### Access the lessons pkg on Github and install in temp library location.
  ### "prefix" puts tmp_lib first (so the package installs there) while keeping
  ### the user's libraries visible, so `remotes` and dependencies are found
  ### without reinstalling anything from CRAN.
  message('Installing package ', pkg, ' version ',
          if(is.null(version)) '(latest)' else version, ' into temp library...')
  withr::with_libpaths(tmp_lib, action = "prefix", {
    remotes::install_github(sprintf("%s/%s", org, pkg), ref = version)
  })
  new_version <- as.character(utils::packageVersion(pkg, lib.loc = tmp_lib))

  #################################
  ### Identify desired lessons ###
  #################################

  ### Identify lessons in current course; lessons available come from the
  ### version just installed in the temp library, not the default library
  lessons_current   <- list.files(here::here(), pattern = "^s[0-9]{2}_.+\\.qmd$")
  lessons_available <- list.files(system.file("lessons", package = pkg, lib.loc = tmp_lib),
                                  full.names = TRUE)

  l_c_df <- data.frame(current_f = lessons_current)
  l_c_df$lesson <- gsub("^s[0-9]{2}_|\\.qmd$", "", basename(lessons_current))
  l_c_df$prefix <- regmatches(lessons_current, m = regexpr("^s[0-9]{2}", lessons_current))

  l_a_df <- data.frame(avail_f = lessons_available)
  l_a_df$lesson <- gsub("^s[0-9]{2}_|\\.qmd$", "", basename(lessons_available))

  l_join_df <- dplyr::inner_join(l_c_df, l_a_df, by = "lesson")
  if(!"all" %in% lessons) {
    ### if any are "all" just update all; if none are "all" then update selected
    ### strip extension just in case
    lessons <- gsub("\\.qmd$", "", lessons)
    ### filter to just those given lessons
    l_join_df <- l_join_df[l_join_df$lesson %in% lessons, ]
  }

  ############################################
  ### Report matched lessons and copy over ###
  ############################################
  if(nrow(l_join_df) == 0) stop("No valid lessons identified for updating!")
  message("Updating lessons to version ", new_version, ":\n  \u2022 ",
          paste(l_join_df$lesson, collapse = "\n  \u2022 "))

  proceed <- readline("Do you wish to update these files? (y/n) ")
  if(tolower(proceed) != "y") {
    stop("Update canceled!")
  }

  ### update files
  copied <- file.copy(from = l_join_df$avail_f, to = here::here(l_join_df$current_f),
                      overwrite = TRUE)
  if(any(!copied)) {
    warning("Some lesson files failed to copy:",
            paste0("\n\u2022  ", l_join_df$current_f[!copied]))
  }

  ### update folders (shared with setup_lessons(); resources included, as in
  ### setup_lessons() and checkin_lessons())
  for(folder in c("images", "data", "slides", "resources")) {
    copy_folders(l_join_df$lesson, from = paste0("lesson_", folder), to = folder,
                 pkg = pkg, lib.loc = tmp_lib, overwrite = TRUE)
  }

  # update _quarto.yml with version flags on updated lines
  yml <- readr::read_file(here::here('_quarto.yml'))
  for(i in seq_len(nrow(l_join_df))) {
    updated_f <- l_join_df$current_f[i]
    ### escape regex metacharacters (e.g., the "." in ".qmd") in the filename
    old_line <- sprintf("%s  ### .+?\\n", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", updated_f))
    new_line <- sprintf("%s  ###  (%s version %s)\n", updated_f, pkg, new_version)
    yml <- sub(old_line, new_line, yml)
  }

  readr::write_file(yml, here::here('_quarto.yml'))

  return(invisible(NULL))
}
