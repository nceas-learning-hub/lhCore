#' Check in modified lessons back into lessons package repository
#'
#' This function creates a new branch of the given lessons repository and copies
#' new or updated lessons over to the repository.  Branch name defaults to the
#' course name but can be overwritten if desired.
#'
#' Lessons can be provided as a character vector of the lesson file names (with or without
#' the prefix/extension ornamentation), or as a dataframe resulting from a call to
#' `compare_diffs()`.  Files copied include the lesson .qmd files, plus any associated
#' image/data/slides folders.
#'
#' Commits then pushes changes back to Github
#'
#' @param lessons A character vector or dataframe of lessons to be checked in
#' @param pkg The package that contains the lessons; here this represents the
#'     repository name for the package.  Default "lhLessons"
#' @param org The GitHub organization where the lessons package repo lives.  Default "nceas-learning-hub"
#' @param branch Optional, a branch name to push lesson changes to.  Default `NULL`
#'     will result in the course project name (project root dir name) being
#'     used as the branch name
#'
#' @export

checkin_lessons <- function(lessons = NULL,
                            pkg = 'lhLessons',
                            org = 'nceas-learning-hub',
                            branch = NULL) {

  if(!verify_course_repo(query = 'Check in lessons from this course?')) {
    message('Canceling check in...')
    return()
  }

  ### Set branch to be the name of the course, unless otherwise specified
  ### should this come from course root dir name? or the default branch of the
  ### package repo?
  course_name <- basename(here::here())
  if(is.null(branch)) branch <- course_name

  ### Resolve the lessons argument (depending on type) into list of local lesson files
  lessons_df <- resolve_lessons(lessons)

  ### check with the user to make sure it's all good!
  message('Retrieved lessons to check in:', paste0('\n\u2022  ', basename(lessons_df$local)))
  continue <- readline('Continue with checking in these lessons? (y/n) ')
  if(tolower(continue) != 'y') {
    message('Canceling lesson check in!')
    return()
  }

  ### message about needing write access to the repo

  ### check out repo to temp dir and create (or identify) branch
  repo_tmp_dir <- checkout_tmp(pkg = pkg, org = org, branch = branch)

  ### set up an on.exit() to delete temp dir in case something errors out
  on.exit(unlink(repo_tmp_dir, recursive = TRUE), add = TRUE)

  ### copy relevant files over; overwrite existing versions incl entire folders
  copy_lessons_to_checkin(lessons_df, repo_tmp_dir)
  copy_files_to_checkin(lessons_df, 'slides',    repo_tmp_dir)
  copy_files_to_checkin(lessons_df, 'data',      repo_tmp_dir)
  copy_files_to_checkin(lessons_df, 'images',    repo_tmp_dir)
  copy_files_to_checkin(lessons_df, 'resources', repo_tmp_dir)

  ### stage all changes (new, modified and deleted files), then check with the user.
  ### All gert calls point at the temp repo via `repo`, so the working directory
  ### is never changed.
  gert::git_add('.', repo = repo_tmp_dir)
  changes <- gert::git_status(staged = TRUE, repo = repo_tmp_dir)
  if(nrow(changes) == 0) stop('No changes to check in: the lessons match the repository already!')
  message('Changes to be committed:',
          paste0('\n\u2022  ', changes$status, ': ', changes$file))
  continue <- readline('Ready to commit these changes? (y/n) ')
  if(tolower(continue) != 'y') {
    stop('Aborting lesson check in!')
  }

  ### commit, then pull (in case others pushed to this branch) and push
  msg <- paste0('lhCore::checkin_lessons(): Checking in lessons from course ',
                course_name, ' to branch ', branch)
  gert::git_commit(msg, repo = repo_tmp_dir)
  gert::git_pull(repo = repo_tmp_dir)
  gert::git_push(repo = repo_tmp_dir)
}

resolve_lessons <- function(lessons) {
  if(is.null(lessons)) {
    ### if NULL, check *all* local lessons - identified by prefix "sXX_"
    lessons_local <- list.files(here::here(),
                                pattern = '^s[0-9]{2}_',
                                full.names = TRUE)
  } else if(is.character(lessons)) {
    ### if character, match local files to those in lessons vector
    lessons_local <- list.files(here::here(),
                                pattern = paste0(lessons, collapse = '|'),
                                full.names = TRUE)
    if(length(lessons_local) != length(lessons)) {
      warning(length(lessons), ' lessons requested, ', length(lessons_local), ' lessons found!')
    }
  } else if(is.data.frame(lessons)) {
    ### If data.frame, check that it matches output from compare_diffs():
    if(!all(c('file_local', 'status') %in% names(lessons))) {
      stop('Dataframe input to checkin_lessons() must be output from compare_diffs(),',
           '\n  looking for column names file_local and status')
    }
    ### drop any unchanged lessons
    changes <- lessons[lessons$status != 0, ]
    lessons_local <- changes$file_local
  } else {
    stop('`lessons` must be NULL, a character vector, or a dataframe from compare_diffs()')
  }

  lessons_df <- data.frame(local = lessons_local)
  lessons_df$remote <- stringr::str_remove(basename(lessons_local), '^s[0-9]{2}_')
  lessons_df$name <- stringr::str_remove(lessons_df$remote, '\\.[qQrR]?md$')

  return(lessons_df)

}

checkout_tmp <- function(pkg, org, branch,
                         url = sprintf("https://github.com/%s/%s", org, pkg)) {
  ### pkg <- 'lhLessons'; org = 'nceas-learning-hub'
  ### (url can be overridden, e.g., to a local bare repo for testing)

  ### Set up temp folder for lessons repository
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  ### clean up the temp dir on any failure (not on success, since the caller needs it)
  success <- FALSE
  withr::defer(if(!success) unlink(tmp_dir, recursive = TRUE))

  ### clone repo into temp folder (gert errors if the clone fails)
  clone_dir <- file.path(tmp_dir, pkg)
  gert::git_clone(url, path = clone_dir, verbose = FALSE)

  ### Check out the branch: use the local branch if it exists; if it only exists
  ### on origin, create a local branch from it; otherwise create a new branch
  ### from the current HEAD
  branches <- gert::git_branch_list(repo = clone_dir)
  local_branches  <- branches$name[branches$local]
  remote_branches <- sub('^[^/]+/', '', branches$name[!branches$local])
  if(branch %in% local_branches) {
    gert::git_branch_checkout(branch, repo = clone_dir)
  } else if(branch %in% remote_branches) {
    gert::git_branch_create(branch, ref = paste0('origin/', branch),
                            checkout = TRUE, repo = clone_dir)
  } else {
    gert::git_branch_create(branch, checkout = TRUE, repo = clone_dir)
  }

  ### Connect local to origin (publishes the branch if it is new)
  gert::git_push(remote = 'origin', set_upstream = TRUE, repo = clone_dir)

  success <- TRUE
  return(clone_dir)

}

copy_lessons_to_checkin <- function(lessons_df, tmp_dir) {
  from <- lessons_df$local
  to   <- file.path(tmp_dir, 'inst', 'lessons', lessons_df$remote)
  x <- file.copy(from = from, to = to, overwrite = TRUE, copy.date = TRUE)
  if(any(!x)) stop('Uh oh, something failed to copy:', paste0('\n\u2022  ', basename(from[!x])))
}

copy_files_to_checkin <- function(lessons_df, folder, tmp_dir) {
  ### get a list of local folders that match lesson names
  ### (pattern must be a single string, so collapse all lesson names into one regex)
  from_dir <- list.files(here::here(folder),
                         pattern = paste0(lessons_df$name, collapse = '|'),
                         full.names = TRUE)
  to_dir   <- file.path(tmp_dir, 'inst', paste0('lesson_', folder), basename(from_dir))
  if(length(from_dir) == 0) {
    message('No ', folder, ' folders to be checked in!')
    return(character(0))
  }

  ### delete existing files; set up new dir if necessary
  x <- lapply(to_dir, function(f) {
    ### f <- to_dir[1]
    if(file.exists(f)) {
      old_fs <- list.files(f, full.names = TRUE, recursive = TRUE)
      unlink(old_fs, recursive = TRUE)
    } else {
      dir.create(f, recursive = TRUE)
    }
  })

  ### pair each local file with its destination, one lesson folder at a time
  ### (so files map to the right folder when there are several lessons)
  file_map <- lapply(seq_along(from_dir), function(i) {
    rel <- list.files(from_dir[i], recursive = TRUE)
    data.frame(from = file.path(from_dir[i], rel),
               to   = file.path(to_dir[i], rel))
  }) |>
    do.call(what = rbind)
  from_fs <- file_map$from
  to_fs   <- file_map$to

  ### build "to" directory structure
  to_dir_structure <- to_fs |> dirname() |> unique() |> sort()
  if(any(!dir.exists(to_dir_structure))) {
    x <- lapply(to_dir_structure[!dir.exists(to_dir_structure)], dir.create, recursive = TRUE)
  }
  ### copy relevant files to new structure
  y <- file.copy(from = from_fs, to = to_fs, overwrite = TRUE, copy.date = TRUE)

  ### one last check!
  if(any(!y)) stop('Uh oh, something failed to copy:', paste0('\n\u2022  ', basename(from_fs[!y])))

  return(to_fs)
}
