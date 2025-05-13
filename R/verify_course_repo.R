#' Verify that the current working directory is the correct course!
#'
#' @param query A string to prompt the user with, generally related to the action
#'     being verified, e.g., 'Do you want to continue with this action in this location?'
#'
#' @returns NULL
verify_course_repo <- function(query = 'Is this the correct course location?') {
  here_dir <- here::here()

  query_string <- paste0('Current project = ', here_dir, '... ', query, ' (y/n) ')
  continue <- readline(query_string)

  if(tolower(continue) != 'y') {
    stop('Please set the working directory to the course repo and try again.')
  }

  return(NULL)

}
