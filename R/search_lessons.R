#' Search available lessons using partial matching and/or tags
#'
#' Identify lessons whose file name matches a given search string and/or
#' whose YAML `categories` field matches one or more tags. Regular
#' expressions are allowed in `query`.
#'
#' @param query A character string to search across lesson file names in
#'     the given package. Regular expressions are allowed. If NULL, all
#'     lessons pass this filter.
#' @param tags A character vector of tags to match against each lesson's
#'     YAML `categories:` field. If NULL, all lessons pass this filter.
#' @param match When `tags` is provided, whether a lesson must match "any"
#'     (default) of the given tags, or "all" of them.
#' @param pkg The package to query for lesson availability (default `lhLessons`)
#' @param quiet Provide progress and diagnostic messages during search?
#'     Default `TRUE`.
#'
#' @return A data frame containing the file names (.qmd) of lessons from
#'     the given package that match the query and/or tags, including a
#'     `categories` column listing each lesson's tags (empty if untagged).
#' @export
#'
#' @examples \dontrun{
#' search_lessons(query = "github")
#' search_lessons(tags = c("beginner", "r"))                # matches either tag
#' search_lessons(tags = c("beginner", "r"), match = "all") # must have both
#' }
search_lessons <- function(query = NULL, tags = NULL, pkg = 'lhLessons',
                           match = c('any', 'all'), quiet = TRUE) {
  match <- match.arg(match)
  v <- utils::packageVersion(pkg) |> paste(collapse = '.')

  if(!quiet) {
    msg_bits <- c(
      if(!is.null(query)) sprintf('name matching \"%s\"', query),
      if(!is.null(tags))  sprintf('tags (%s): %s', match, paste(tags, collapse = ', '))
    )
    if(length(msg_bits) == 0) {
      message('Gathering all available lessons from ', pkg, ' version ', v)
    } else {
      message('Searching lessons from ', pkg, ' version ', v,
              ' where ', paste(msg_bits, collapse = ' and '))
    }
  }

  l_vec <- list.files(system.file('lessons', package = pkg), full.names = TRUE)
  l_df  <- data.frame(lesson_file = l_vec,
                      lesson = sub('\\.[qQrR]md$', '', basename(l_vec)))

  ### filename-based filtering
  if(is.null(query)) {
    keep_query <- rep(TRUE, nrow(l_df))
  } else {
    lesson_txt <- gsub("[^a-z0-9]", " ", tolower(l_df$lesson))
    query_str <- paste(tolower(query), collapse = '|')
    keep_query <- grepl(query_str, lesson_txt) |
                  grepl(query_str, basename(l_vec))
  }

  ### pull categories from each lesson's YAML, always (useful for browsing)
  l_df$categories <- lapply(l_df$lesson_file, get_lesson_categories)

  ### tag-based filtering
  if(is.null(tags)) {
    keep_tags <- rep(TRUE, nrow(l_df))
  } else {
    tags_lower <- tolower(tags)
    keep_tags <- vapply(l_df$categories, function(cats) {
      cats_lower <- tolower(cats)
      if(match == 'any') any(tags_lower %in% cats_lower) else all(tags_lower %in% cats_lower)
    }, logical(1))
  }

  result_df <- l_df[keep_query & keep_tags, ]

  if(nrow(result_df) == 0) {
    warning('Note: no lessons in ', pkg, ' version ', v, ' match the given search criteria...')
  }

  return(result_df)
}

### not exported
get_lesson_categories <- function(path) {
  yml <- tryCatch(rmarkdown::yaml_front_matter(path), error = function(e) NULL)
  if(is.null(yml$categories)) return(character(0))
  as.character(yml$categories)
}