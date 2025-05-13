check_git_steps <- function() {
  is_git <- dir.exists(here::here('.git'))
  if(is_git) {
    is_remote <- any(stringr::str_detect(system("git remote -v", intern = TRUE), "^origin"))
  } else {
    is_remote <- FALSE
  }

  course_org <- get_course_metadata()["course_org"]

  if(!is_git) {
    message("This project does not seem to be a Git repository.  You might want to:",
            "\n  \u2022 Use `usethis::use_git()` to set up your project as a Git-tracked project, and then...",
            "\n  \u2022 Use `usethis::use_github(organisation = '", course_org,
            "')` to connect the project with Github!\n")
  } else if(!is_remote) {
    message("This Git-tracked project does not seem to be connected to GitHub.  You might want to:",
            "\n  \u2022 Use `usethis::use_github(organisation = '", course_org,
            "')` to connect the project with Github!\n")
  }

}
