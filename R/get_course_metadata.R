#' Get course metadata
#'
#' Course metadata is set up in init_course, to contain info on the course name,
#' repo name, organization, description, and course dates.
#'
#' @returns
#' A named vector of metadata fields and values

get_course_metadata<- function() {
  ### get metadata from metadata_course.csv
  metadata_df <- readr::read_csv(here::here("metadata_course.csv"))
  meta <- metadata_df$value
  names(meta) <- metadata_df$field

  return(meta)
}
