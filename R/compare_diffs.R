#' Compare differences between local lesson files and version in lessons package
#'
#' This function uses `git diff` to compare the local versions of lesson to their
#' counterpart within a given lessons package (default `lhLessons`).  Some notes:
#'
#' * This compares using the (bare) lesson name (e.g., `s02_git_setup.qmd` is compared
#'   to the package lesson named `git_setup.qmd`), so if the course designer has changed
#'   a lesson filename manually (beyond the prefix), that file will NOT be compared, and assumed to be
#'   a new lesson entirely.
#' * Changes in auxiliary files, e.g., images, slides, or data, will NOT be compared.
#'   However, if the path to these files change (e.g., new or renamed image), those
#'   changes in the lesson file will be noted.
#' * The lessons package is installed fresh from GitHub into a temporary library
#'   (removed afterwards) and compared against, so the comparison never depends
#'   on which version you happen to have installed.  By default this is the latest
#'   version on the default branch of the lessons repository (looked up on GitHub,
#'   not assumed to be `main`); use `branch` or `pkg_version` to compare against
#'   something else.
#'
#'
#' @param pkg A character name of the lessons package, default `lhLessons`.
#' @param org Character Github org (or user) where the package lives, default `nceas-learning-hub`.
#' @param pkg_version Optional version to compare against: a tag, commit, or
#'     version as accepted by `remotes::install_github()`'s `ref`.  Default `NULL`
#'     uses `branch`.  If both are given, `pkg_version` is used.
#' @param branch A character name of the branch to compare against.  Default `NULL`
#'     looks up the default branch of the lessons repository.
#' @param prefix A character indicator of the expected prefix letter for lesson files.
#' @param file_out The location in the current repository where the diff results will
#'     be saved as a text file.
#'
#' @returns (invisibly) a data frame containing `file_remote`, `file_local`,
#'     `result` (of `git diff`) and `status`.  Writes results out to a text file
#'     (default `git_diff.txt`).  Note `file_remote` points into the temporary
#'     library, which no longer exists once the function returns.
#' @export
#'
#' @examples \dontrun{
#'   compare_diffs()
#' }

compare_diffs <- function(pkg = 'lhLessons',
                          org = 'nceas-learning-hub',
                          pkg_version = NULL,
                          branch = NULL,
                          prefix = 's', file_out = 'git_diff.txt') {

  ### decide what to compare against: a specific version/tag/commit, a named
  ### branch, or (default) the lessons repo's default branch, looked up rather
  ### than assumed to be "main"
  if(is.null(pkg_version) && is.null(branch)) {
    branch <- get_default_branch(pkg, org = org)
    if(is.na(branch)) branch <- NULL ### remote reports none; install_github() uses its default
  }
  ref <- if(!is.null(pkg_version)) pkg_version else branch
  ref_label <- if(!is.null(pkg_version)) {
    paste0('version ', pkg_version)
  } else if(!is.null(branch)) {
    paste0('branch ', branch)
  } else {
    'default branch'
  }

  ### install that version into a temp library, removed on exit (even on error)
  tmp_lib <- tempfile()
  dir.create(tmp_lib)
  on.exit(unlink(tmp_lib, recursive = TRUE), add = TRUE)

  message('Installing ', pkg, ' (', ref_label, ') from GitHub into a temporary library...')
  ### "prefix" installs into tmp_lib while keeping the user's libraries visible
  ### for dependencies; upgrade = "never" avoids prompts about other packages
  withr::with_libpaths(tmp_lib, action = 'prefix', {
    remotes::install_github(sprintf('%s/%s', org, pkg), ref = ref,
                            upgrade = 'never', quiet = TRUE)
  })
  ### the package name at an older tag can differ from `pkg` (e.g., v0.0.2
  ### installs as "coreRlessons"), so find what actually landed in tmp_lib
  pkg_src <- list.files(tmp_lib)
  if(length(pkg_src) != 1) {
    stop('Expected exactly one package installed in the temp library from ', org, '/', pkg,
         ' (', ref_label, '), found: ',
         if(length(pkg_src) == 0) 'none' else paste(pkg_src, collapse = ', '))
  }
  if(pkg_src != pkg) message("Note: this version installs as package '", pkg_src, "', not '", pkg, "'.")
  new_version <- as.character(utils::packageVersion(pkg_src, lib.loc = tmp_lib))
  message('Comparing local lessons to ', pkg, ' ', new_version, ' (', ref_label, ')')

  ### check *all* local lessons - identified by appropriate prefix (ignore 00)
  lessons_local <- list.files(here::here(), pattern = paste0('^', prefix, '[0-9]{2}_'), full.names = TRUE)
  lessons_local <- lessons_local[!grepl(paste0('^', prefix, '00_'), basename(lessons_local))]

  ### Clean lesson filenames to bare lesson names
  make_bare <- function(x) {
    gsub(paste0('^', prefix, '[0-9]{2}_|\\.[qQrR]?md$'), '', basename(x))
  }
  lessons_df <- data.frame(file_local = lessons_local,
                           bare = make_bare(lessons_local))

  ### Check local lessons against those in package via search_lessons(),
  ### so any local lessons not in package might indicate new lessons.
  lessons_avail <- search_lessons(pkg = pkg_src, lib.loc = tmp_lib)

  ### New lessons: add result, status to match git diff output
  lesson_new <- lessons_df[!lessons_df$bare %in% lessons_avail$lesson, ]
  new_lesson_df <- NULL
  if(nrow(lesson_new) > 0) {
    warning('Lesson(s) detected in course but not listed in package: potentially new?',
            paste('\n  \u2022 ', lesson_new$bare, collapse = '\n  \u2022'))
    new_lesson_df <- data.frame(file_remote = NA_character_,
                                file_local = lesson_new$file_local,
                                result = 'New file detected',
                                status = 2)
  }

  ### Lessons to compare differences:
  lessons_compare <- lessons_avail |>
    dplyr::inner_join(lessons_df, by = c('lesson' = 'bare'))

  ### process git diffs, write to .txt, and report changed lessons to user
  diffs_df <- purrr::map2(.x = lessons_compare$lesson_file,
                          .y = lessons_compare$file_local,
                          .f = git_diff) |>
    dplyr::bind_rows()

  ### append new lessons, if they have been detected
  diffs_df <- dplyr::bind_rows(diffs_df, new_lesson_df)

  diffs_txt <- paste(basename(diffs_df$file_local), '\n\n', diffs_df$result,
                     collapse = '\n\n\n====================\n\n\n')

  readr::write_file(diffs_txt, here::here(file_out))

  ### Report out files with changes
  files_changed <- diffs_df$file_local[diffs_df$status == 1] |> basename()
  out_msg <- if(length(files_changed) == 0) {
    'No file differences detected!'
  } else {
    sprintf('Files with changes detected: \n  \u2022  %s',
            paste0(files_changed, collapse = '\n  \u2022  '))
  }
  message(out_msg)

  ### Report out new files
  files_new <- diffs_df$file_local[diffs_df$status == 2] |> basename()
  out_msg <- if(length(files_new) == 0) {
    'No new files detected!'
  } else {
    sprintf('New files detected: \n  \u2022  %s',
            paste0(files_new, collapse = '\n  \u2022  '))
  }
  message(out_msg)

  return(invisible(diffs_df))
}

git_diff <- function(f_remote, f_local, word_diff = FALSE) {
  ### `git diff --no-index` exits 1 for a missing file just as it does for a real
  ### difference, so check explicitly rather than report a false "changed"
  missing_f <- c(f_remote, f_local)[!file.exists(c(f_remote, f_local))]
  if(length(missing_f) > 0) stop('Cannot compare, file not found: ', paste(missing_f, collapse = ', '))

  w_d <- if(word_diff) '--word-diff' else character(0)
  diff_args <- c('diff', '--no-index', w_d, shQuote(f_remote), shQuote(f_local))

  ### returns a vector of lines from `git diff`.  stderr is discarded because on
  ### Windows it gets mixed into the captured output, and git warnings there
  ### (e.g., "LF will be replaced by CRLF") would be mistaken for differences.
  ### `git diff --no-index` exits 0 if the files match, 1 if they differ (which
  ### system2() warns about), and higher on a real error; the exit code is only
  ### attached as the "status" attribute when non-zero.
  diff_vec <- suppressWarnings(system2('git', diff_args, stdout = TRUE, stderr = FALSE))
  status <- attr(diff_vec, 'status')
  if(is.null(status)) status <- 0
  if(status > 1) stop('git diff failed (exit status ', status, ') comparing ', f_remote, ' and ', f_local)

  if(status == 0) {
    ### No differences!
    result <- 'No difference detected'
  } else {
    ### collapse into a single string with line breaks
    result <- paste0(diff_vec, collapse = '\n')
  }

  out_df <- data.frame(file_remote = f_remote, file_local = f_local, result, status)
  return(out_df)

}
