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
#' @param pkg
#' @param org
#' @param branch
#'
#' @returns
#' @export
#'
#' @examples
checkin_lessons <- function(lessons = NULL,
                            pkg = 'lhLessons',
                            org = 'nceas-learning-hub',
                            branch = NULL) {

  verify_course_repo(query = 'Check in lessons from this course?')

  ### Set branch to be the name of the course, unless otherwise specified
  ### should this come from course metadata, project directory name, elsewhere?
  meta <- get_course_metadata()
  if(is.null(branch)) branch <- meta['course_proj']

  ### Resolve the lessons argument (depending on type) into list of local lesson files
  lessons_df <- resolve_lessons(lessons)

  ### check with the user to make sure it's all good!
  message('Retrieved lessons to check in:', paste0('\n\u2022  ', basename(lessons_df$local)))
  continue <- readline('Continue with checking in these lessons? (y/n)')
  if(tolower(continue) != 'y') {
    stop('Aborting lesson check in!')
  }

  ### message about needing write access to the repo

  ### check out repo to temp dir and create (or identify) branch
  repo_tmp_dir <- checkout_tmp(pkg = pkg, org = org, branch = branch)

  ### set up an on.exit() to delete temp dir in case something errors out
  on.exit(unlink(repo_tmp_dir, recursive = TRUE))

  ### copy relevant files over; overwrite existing versions incl entire folders
  copy_lessons_to_checkin(lessons_df, repo_tmp_dir)
  copy_files_to_checkin(lessons_df, 'slides', repo_tmp_dir)
  copy_files_to_checkin(lessons_df, 'data',   repo_tmp_dir)
  copy_files_to_checkin(lessons_df, 'images', repo_tmp_dir)

  ### check with user after git add --all
  course_repo <- here::here()
  setwd(repo_tmp_dir)
  x <- system('git add --all')
  msg <- system('git status', intern = TRUE)
  message(paste0(msg, collapse = '\n'))
  continue <- readline('Ready to commit these changes? (y/n) ')
  if(tolower(continue) != 'y') {
    stop('Aborting lesson check in!')
  }

  ### commit changes
  msg <- paste0('lhCore::checkin_lessons(): Checking in lessons from course ',
                meta['course_proj'], ' to branch ', branch)
  x <- system(sprintf('git commit -m "%s"', msg))
  x <- system('git pull')
  if(x != 0) {
    stop('Uh oh, error when pulling remote!')
  }
  x <- system('git push')

  setwd(course_repo)

  ### delete temp dir
  unlink(repo_tmp_dir, recursive = TRUE)
}

resolve_lessons <- function(lessons) {
  if(is.null(lessons)) {
    ### if NULL, check *all* local lessons - identified by prefix "sXX_"
    lessons_local <- list.files(here::here(),
                                pattern = '^s[0-9]{2}_',
                                full.names = TRUE)
  }
  if(is.character(lessons)) {
    ### if character, match local files to those in lessons vector
    lessons_local <- list.files(here::here(),
                                pattern = paste0(lessons, collapse = '|'),
                                full.names = TRUE)
    if(length(lessons_local) != length(lessons)) {
      warning(length(lessons), ' lessons requested, ', length(lessons_local), ' lessons found!')
    }
  }
  if(is.data.frame(lessons)) {
    ### If data.frame, check that it matches output from compare_diffs():
    if(any(!names(lessons) %in% c('file_remote', 'file_local', 'result', 'status'))) {
      stop('Dataframe input to checkin_lessons() must be output from compare_diffs(),',
           '\n  looking for column names file_remote, file_local, result, status')
    }
    ### drop any unchanged lessons
    changes <- lessons[lessons$status != 0, ]
    lessons_local <- changes$file_local
  }

  lessons_df <- data.frame(local = lessons_local)
  lessons_df$remote <- stringr::str_remove(basename(lessons_local), '^s[0-9]{2}_')
  lessons_df$name <- stringr::str_remove(lessons_df$remote, '..md$')

  return(lessons_df)

}

checkout_tmp <- function(pkg, org, branch) {
  ### pkg <- 'lhLessons'; org = 'nceas-learning-hub'

  ### Set up temp folder for lessons repository
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  ### clone repo into temp folder
  clone_str <- sprintf("git clone https://github.com/%s/%s %s/%s", org, pkg, tmp_dir, pkg)
  x <- system(clone_str)

  if(x != 0) {
    x <- system(sprintf('rm -rf %s', tmp_dir))
    stop('Oops, an error occurred during git clone!')
  }

  ### Navigate to that dir
  course_dir <- here::here()
  setwd(file.path(tmp_dir, pkg))

  ### create branch if needed and check out
  branch_check <- system('git branch -a', intern = TRUE) |>
    stringr::str_remove('^.+/')
  b_flag <- ifelse(!branch %in% branch_check, '-b', '')
  x <- system(sprintf('git checkout %s %s', b_flag, branch))

  if(x != 0) {
    ### this doesn't work - returns an error:
    ### /usr/bin/rm: cannot remove 'C:\...\file2b4849cd7c82/lhLessons': Device or resource busy
    x <- system(sprintf('rm -rf %s', tmp_dir))
    stop('Oops, an error occurred during git checkout!')
  }

  ### Connect local to origin
  x <- system(sprintf('git push -u origin %s', branch))
  if(x != 0) {
    ### this doesn't work - returns an error:
    ### /usr/bin/rm: cannot remove 'C:\...\file2b4849cd7c82/lhLessons': Device or resource busy
    x <- system(sprintf('rm -rf %s', tmp_dir))
    stop('Oops, an error occurred during git push -u origin!')
  }

  ### set back to project home and return temp directory/repo name
  setwd(course_dir)
  return(file.path(tmp_dir, pkg))

}

copy_lessons_to_checkin <- function(lessons_df, tmp_dir) {
  from <- lessons_df$local
  to   <- file.path(tmp_dir, 'inst', 'lessons', lessons_df$remote)
  x <- file.copy(from = from, to = to, overwrite = TRUE, copy.date = TRUE)
  if(any(!x)) stop('Uh oh, something failed to copy:', paste0('\n\u2022  ', basename(from[!x])))
}

copy_files_to_checkin <- function(lessons_df, folder, tmp_dir) {
  ### get a list of local folders that match lesson names
  from_dir <- list.files(here::here(folder), pattern = lessons_df$name, full.names = TRUE)
  to_dir   <- file.path(tmp_dir, 'inst', paste0('lesson_', folder), basename(from_dir))
  if(length(from_dir) == 0) message('No ', folder, ' folders to be checked in!')

  ### delete existing files; set up new dir if necessary
  x <- lapply(to_dir, function(f) {
    if(file.exists(f)) {
      old_fs <- list.files(f, full.names = TRUE, recursive = TRUE)
      unlink(old_fs, recursive = TRUE)
    } else {
      dir.create(f)
    }
  })

  ### get indiv files from local and copy to temp dir
  from_fs <- list.files(from_dir, recursive = TRUE, full.names = TRUE)
  x <- file.copy(from = from_fs, to = to_dir, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)

  ### one last check!
  if(any(!x)) stop('Uh oh, something failed to copy:', paste0('\n\u2022  ', basename(from[!x])))
}
