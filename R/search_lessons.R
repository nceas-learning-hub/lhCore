#' Search available lessons using partial matching
#'
#' Identify lessons whose file name matches a given search string.  Regular
#' expressions are allowed.
#'
#' @param query A character string to search across lessons in the given
#'     package.  Regular expressions are allowed.  If no query given, returns all available lessons.
#' @param pkg The package to query for lesson availability (default `lhLessons`)
#' @param quiet Provide progress and diagnostic messages during search?
#'     Default `TRUE`.
#'
#' @return A data frame containing the file names (.qmd) of lessons
#'     from the given package that match the search query.
#' @export
#'
#' @examples \dontrun{search_lessons(query = "github"))}

search_lessons <- function(query = NULL, pkg = 'lhLessons', quiet = TRUE) {
  v <- utils::packageVersion(pkg) |> paste(collapse = '.')
  if(!quiet) {
    if(is.null(query)) message('Gathering all available lessons from ', pkg, ' version ', v)
    else message('Searching available lessons from ', pkg, ' version ', v, ' that match \"', query, '\"')
  }

  l_vec <- list.files(system.file('lessons', package = pkg),
                      full.names = TRUE)
  l_df <- data.frame(lesson_file = l_vec,
                     lesson = stringr::str_remove(basename(l_vec), '..md$'))

  ### change lesson to a sentence format just in case
  lesson_txt <- stringr::str_replace_all(tolower(l_df$lesson), "[^a-z0-9]", " ")

  query <- tolower(query)
  keep_vec <- stringr::str_detect(lesson_txt, query) |
    stringr::str_detect(basename(l_vec), query)

  result_df <- l_df[keep_vec, ]

  if(nrow(result_df) == 0) {
      warning('Note: no lessons are available in ', pkg, ' version ', v, ' that match \"', query, '\"...')
    }


  return(result_df)
}
