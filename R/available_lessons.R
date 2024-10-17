#' Get a dataframe of available lessons
#'
#' @param lessons An optional character vector to query whether specific lessons
#'     are available
#'
#' @return A data frame containing the file names (.qmd) of all (or selected)
#'     lessons from the coreRlessons package.  Eventually, including file title, date, and
#'     author (if available in the .qmd)
#' @export
#'
#' @examples \dontrun{available_lessons(lessons = c('r_functions.qmd', 'this_package_does_not_exist.Rmd'))}

available_lessons <- function(lessons = NULL) {
  v <- packageVersion('coreRlessons') |> paste(collapse = '.')
  message('Retrieving available lessons from coreRlessons version ', v)

  l_cln <- stringr::str_remove(lessons, '..md$') ### drop qmd or rmd extension

  l_vec <- list.files(system.file('lessons', package = 'coreRlessons'),
                      full.names = TRUE)
  l_df <- data.frame(lesson_file = l_vec,
                     lesson = stringr::str_remove(basename(l_vec), '..md$'))

  if(!is.null(lessons)) {
    ### id lessons provided by user, that aren't available
    missing <- l_cln[!l_cln %in% l_df$lesson]
    ### filter dataframe to user-provided lessons only
    l_df <- l_df[l_df$lesson %in% l_cln, ]
    warning('Note: the following lessons are not available in coreRlessons version ', v,
            paste('\n  \u25CF', missing))
  }

  return(l_df)
}
