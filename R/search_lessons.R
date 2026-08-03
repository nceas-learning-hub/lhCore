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

  if(is.null(query)) query <- '.'

  l_vec <- list.files(system.file('lessons', package = pkg),
                      full.names = TRUE)
  l_df <- data.frame(lesson_file = l_vec,
                     lesson = sub('..md$', '', basename(l_vec)))

  ### change lesson to a sentence format just in case
  lesson_txt <- gsub("[^a-z0-9]", " ", tolower(l_df$lesson))

  ### create query string as collapsed vector of OR clauses
  query_str <- tolower(query) |> paste(collapse = '|')
  keep_vec <- grepl(query_str, lesson_txt) |
    grepl(query_str, basename(l_vec))

  result_df <- l_df[keep_vec, ]

  if(nrow(result_df) == 0) {
    warning('Note: no lessons are available in ', pkg, ' version ', v, ' that match \"', query_str, '\"...')
  }

  return(result_df)
}
