#' Update lessons in a lhCore course
#'
#' @section Warning:
#' This function is **destructive**: it overwrites the matching lesson qmd files
#' and lesson subfolders in `images`, `data`, `slides`, and `resources` with the
#' versions from the lessons package, permanently discarding any local edits or
#' customizations to those lessons.  Use with caution.  Before updating, commit
#' your work to Git and run `compare_diffs()` to see how your local lessons differ
#' from the lessons package.
#'
#' @param lessons A vector of specific lessons to be updated, based on file
#'     names (bare or with `.qmd` extension).  If lesson names don't match
#'     lessons in the source book, an error will be returned.  The default `all`
#'     will update all available lessons, i.e., those whose lesson names match
#'     those in the source book.  Customized lessons, whose names do not match
#'     those in the source book, will not be updated, even with `version = 'all'`.
#' @param pkg Name of the package where lessons are curated, default "lhLessons"
#' @param version A version of the `pkg` package from which the updated lessons
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
  check_interactive()
  check_lhlessons_compatibility(pkg = pkg)
  ### placeholder - give a vector of lesson names, optionally give a version tag
  ### if version tag is NULL use latest otherwise use whatever the user supplied
  ### in the `@ref` portion - could be a commit, tag, branch name
  ### install_github("nceas-learning-hub", "lhLessons@v0.1.1")

  ### if version is not current, install lhLessons from requested version
  ### identify lessons to be updated - which match the names in lhLessons at this version
  ### copy over into sections folder, overwriting old versions

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
    ### upgrade = "never" avoids prompts about updating other packages
    remotes::install_github(sprintf("%s/%s", org, pkg), ref = version, upgrade = "never")
  })
  ### the package name at an older tag can differ from the current `pkg` (e.g.,
  ### v0.0.2 installs as "coreRlessons"), so find what actually landed in tmp_lib
  pkg_src <- list.files(tmp_lib)
  if(length(pkg_src) != 1) {
    stop("Expected exactly one package installed in the temp library from ", org, "/", pkg,
         " (version ", if(is.null(version)) 'latest' else version, "), found: ",
         if(length(pkg_src) == 0) "none" else paste(pkg_src, collapse = ", "))
  }
  if(pkg_src != pkg) message("Note: this version installs as package '", pkg_src, "', not '", pkg, "'.")
  new_version <- as.character(utils::packageVersion(pkg_src, lib.loc = tmp_lib))

  #################################
  ### Identify desired lessons ###
  #################################

  ### Identify lessons in current course; lessons available come from the
  ### version just installed in the temp library, not the default library
  lessons_current   <- list.files(here::here(), pattern = "^s[0-9]{2}_.+\\.qmd$")
  lessons_available <- list.files(system.file("lessons", package = pkg_src, lib.loc = tmp_lib),
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
    ### error on any requested lesson that isn't both in the course and in the
    ### source package version (nothing is changed yet)
    lessons_unmatched <- setdiff(lessons, l_join_df$lesson)
    if(length(lessons_unmatched) > 0) {
      stop("Requested lessons not found in both the course and ", pkg, " version ", new_version, ":",
           paste0("\n\u25CF ", lessons_unmatched))
    }
    ### filter to just those given lessons
    l_join_df <- l_join_df[l_join_df$lesson %in% lessons, ]
  }

  ############################################
  ### Report matched lessons and copy over ###
  ############################################
  if(nrow(l_join_df) == 0) stop("No valid lessons identified for updating!")

  ### warn if selected lessons are already recorded at this version
  meta_f <- here::here("metadata_lessons.csv")
  if(file.exists(meta_f)) {
    meta_old <- readr::read_csv(meta_f, col_types = readr::cols(.default = "c"))
    if(all(c("lesson", "pkg_version") %in% names(meta_old))) {
      already <- unique(l_join_df$lesson[l_join_df$lesson %in%
                          meta_old$lesson[meta_old$pkg_version %in% new_version]])
      if(length(already) > 0) {
        warning("Already recorded at ", pkg, " version ", new_version, " in metadata_lessons.csv",
                if(length(already) == length(unique(l_join_df$lesson))) " (all selected lessons)" else "",
                ":", paste0("\n\u2022 ", already), call. = FALSE)
      }
    }
  }

  message("\u26A0\uFE0F WARNING: this will OVERWRITE the lesson files and folders below, ",
          "permanently discarding any local edits or customizations.",
          "\n  Commit your work to Git first, and consider running `compare_diffs()` to review ",
          "your local changes against the lessons package.\n")
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
    copy_folders(l_join_df$lesson, from = file.path("lessons", folder), to = folder,
                 pkg = pkg_src, lib.loc = tmp_lib, overwrite = TRUE)
  }

  ### record the new package and version for the updated lessons; read all
  ### columns as character so versions like "1.0" aren't coerced to numbers
  if(file.exists(meta_f)) {
    meta <- readr::read_csv(meta_f, col_types = readr::cols(.default = "c"))
    is_updated <- meta$lesson %in% l_join_df$lesson
    meta$package[is_updated]     <- pkg
    meta$pkg_version[is_updated] <- new_version
    readr::write_csv(meta, meta_f)
  } else {
    warning("metadata_lessons.csv not found; new lesson version (", new_version, ") was not recorded.")
  }

  return(invisible(NULL))
}
