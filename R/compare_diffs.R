#' Compare differences between local lesson files and version in lessons package
#'
#' This function uses `git diff` to compare the local versions of lesson to their
#' counterpart within a given lessons package (default `lhLessons`).  Some notes:
#'
#' * This compares using the (bare) lesson name (e.g., `s02_git_setup.qmd` is compared
#'   to the package lesson named `git_setup.qmd`), so if the course designer has changed
#'   a lesson filename manually, that file will NOT be compared, and assumed to be
#'   a new lesson entirely.
#' * Changes in auxiliary files, e.g., images, slides, or data, will NOT be compared.
#'   However, if the path to these files change (e.g., new or renamed image), those
#'   changes in the lesson file will be noted.
#' * As of now, checks against the `main` branch of the currently installed lessons
#'   package version.  User should confirm that the lessons package version is up to date!
#'
#'
#' @param pkg A character name of the lessons package, default `lhLessons`.
#' @param org Character Github org (or user) where the package lives, default `nceas-learning-hub`.
#' @param pkg_version Character Version tag for package, default NULL indicates latest.
#' @param branch A character name of the branch to be checked.  Currently only checks against `main`.
#' @param prefix A character indicator of the expected prefix letter for lesson files.
#' @param file_out The location in the current repository where the results will
#'     be saved as a text file.
#'
#' @returns (invisibly) a data frame containing `file_remote`, `file_local`,
#'     `result` (of `git diff`) and `status`.  Writes results out to a text file
#'     (default `git_diff.txt`)
#' @export
#'
#' @examples \dontrun{
#'   compare_diffs()
#' }

compare_diffs <- function(pkg = 'lhLessons',
                          org = 'nceas-learning-hub',
                          pkg_version = NULL,
                          branch = 'main',
                          prefix = 's', file_out = 'git_diff.txt') {

  lessons_local <- list.files(here::here(), pattern = paste0(prefix, '[0-9]{2}_'), full.names = TRUE)

  ### Drop lessons with s00_ prefix
  lessons_local <- lessons_local[!grepl('^s00_', basename(lessons_local), ignore.case = TRUE)]

  ### Clean lesson filenames to bare lesson names
  make_bare <- function(x) {
    stringr::str_remove_all(basename(x), paste0('^', prefix, '[0-9]{2}_|\\..md$'))
  }
  lessons_df <- data.frame(file_local = lessons_local,
                           bare = make_bare(lessons_local))

  ### Lessons to compare differences:
  lessons_avail <- search_lessons(pkg = pkg)
  lessons_compare <- lessons_avail |>
    dplyr::inner_join(lessons_df, by = c('lesson' = 'bare'))

  ### New lessons: add result, status to match git diff
  lessons_new <- lessons_df[!lessons_df$bare %in% lessons_avail$lesson, ]
  lessons_new <- lessons_new['file_local']
  lessons_new$result = 'New file'; lessons_new$status = 2

  ### process git diffs, write to .txt, and report changed lessons to user
  diffs_df <- purrr::map2_df(.x = lessons_compare$lesson_file,
                             .y = lessons_compare$file_local,
                             .f = git_diff)
  diffs_df <- diffs_df |>
    dplyr::bind_rows(lessons_new)

  diffs_txt <- paste(basename(diffs_df$file_local), '\n\n', diffs_df$result,
                     collapse = '\n\n\n====================\n\n\n')

  readr::write_file(diffs_txt, here::here(file_out))

  ### Report out files with changes
  files_changed <- diffs_df$file_local[diffs_df$status == 1] |> basename()
  out_msg <- ifelse(length(files_changed) == 0,
                    '\nNo file differences detected!',
                    sprintf('\nFiles with changes detected: \n  \u2022  %s',
                            paste0(files_changed, collapse = '\n  \u2022  ')))
  cat(out_msg)

  ### Report out new files
  files_new <- diffs_df$file_local[diffs_df$status == 2] |> basename()
  out_msg <- ifelse(length(files_new) == 0,
                    '\nNo new files detected!',
                    sprintf('\nNew files detected: \n  \u2022  %s',
                            paste0(files_new, collapse = '\n  \u2022  ')))
  cat(out_msg)


  return(invisible(diffs_df))
}

git_diff <- function(f_remote, f_local, word_diff = FALSE) {
  w_d <- ifelse(word_diff, '--word-diff', '')
  diff_string <- sprintf('git diff --no-index %s %s %s', w_d, f_remote, f_local)

  ### returns a vector of lines returned from `git diff`, status 1 if diff, status 0 if not
  suppressWarnings(diff_vec <- system(diff_string, intern = TRUE))

  if(length(diff_vec) == 0) {
    ### No differences!
    result = 'No difference detected'
    status = 0
  } else {
    status <- attr(diff_vec, 'status')

    ### collapse into a single string with line breaks
    result <- paste0(diff_vec, collapse = '\n')
  }

  out_df <- data.frame(file_remote = f_remote, file_local = f_local, result, status)
  return(out_df)

}
