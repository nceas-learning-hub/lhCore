#' Enable Git and GitHub for course
#'
#' This is effectively a wrapper around `usethis::use_git()` and `usethis::use_github()`,
#' with the small modification that the user is NOT prompted to
#' restart to show the Git pane until AFTER offering to link up to GitHub.  Additionally
#' the default argument for `org` aims the course to be posted on the NCEAS Learning Hub
#' organization, rather than NULL.
#'
#' @param message The commit message to be included when committing untracked files.
#'     Defaults to "Initial commit".
#' @param org The organization where the course will be hosted.
#' @param ... Additional arguments to `usethis::use_github()`.
#'
#' @return TRUE
#' @export
#'
#' @examples
#' \dontrun{gitify(message = 'first commit', org = 'nceas-learning-hub')}
#'
gitify <- function (message = "Initial commit", org = 'nceas-learning-hub', ...)
{
    needs_init <- !(usethis:::uses_git())
    if (needs_init) {
        ui_bullets(c(v = "Initializing Git repo."))
        usethis:::git_init()
        if (Sys.getenv("POSITRON") == "1") {
            Sys.sleep(1)
        }
    }
    use_git_ignore(git_ignore_lines)
    if (usethis:::git_uncommitted(untracked = TRUE)) {
        usethis:::git_ask_commit(message, untracked = TRUE)
    }

    githubify <- readline(paste0('Do you wish to create this course at github.com/', org, '/', usethis:::project_name(), '? (y/n) '))
    if(tolower(githubify) == 'y') usethis::use_github(organisation = org, ...)

    if (needs_init && Sys.getenv("POSITRON") != "1") {
        restart_rstudio("A restart of RStudio is required to activate the Git pane.")
    }
    return(invisible(TRUE))
}
