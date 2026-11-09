#' Set up a schedule of course lessons
#'
#' Create the structure necessary to implement an ordered set of lessons in the Quarto book format.
#' The user provides a character vector of lesson names (filenames, bare or with `.qmd` extension);
#' these and associated materials (images, data) are copied from `lhLessons` package into the
#' course project (created with `init_course()`).  Sets up the index.qmd, _quarto.yml, and some
#' additional files.
#'
#' @section Warning:
#' Using `overwrite = TRUE` is **destructive**: it permanently deletes the existing
#' lesson qmd files and the matching lesson folders in `images`, `data`, `slides`,
#' and `resources` (including any local edits or customizations) and replaces the
#' lesson list in `_quarto.yml`.  Use with caution.  Before overwriting, commit
#' your work to Git and consider running `compare_diffs()` to see how your local
#' lessons differ from the lessons package.
#'
#' @param lessons Information on the lessons to be included in the course, as a
#'     character vector or data.frame.  A character vector containing the names
#'     (bare or .qmd extension) of the lessons, in order; optionally, a named
#'     character vector where the names are course modules (e.g., "Day 1", "Day 2").
#'     Alternately, a data.frame with columns "module" and "lesson", similar to the
#'     named character vector; see `lhLessons::example_course`.
#' @param package The name of the course lessons package to use.
#' @param modules An additional way to provide module information for the lessons:
#'     a vector of module names that matches the length of the lessons vector. Caution:
#'     if `modules` is not NULL, and module names are provided directly in `lessons`,
#'     this argument will take precedence.
#' @param overwrite If there is already an existing lessons structure, and
#'     `overwrite = FALSE`, will return an error.  If `overwrite = TRUE`, after
#'     confirmation the existing lesson qmd files (`sNN_*.qmd`) and the matching
#'     lesson folders in `images`, `data`, `slides`, and `resources` are deleted,
#'     the lesson list in `_quarto.yml` (between the `# START SESSIONS` and
#'     `# END SESSIONS` markers) is replaced, and the new lessons are installed.
#'
#' @return NULL (invisibly)
#'
#' @examples
#' \dontrun{
#' setup_lessons(lessons = c("activity_reproducibility_lego",
#'                           "lecture_tidy_data.qmd",
#'                           "github_collaboration",
#'                           "r_functions.qmd"))
#' ### modules as named vector
#' lesson_mods <- search_lessons()$lesson[1:6] |> setNames(c(1, 1, 2, 2, 3, 3))
#' setup_lessons(lessons = lesson_mods)
#'
#' ### modules as argument
#' setup_lessons(lessons = search_lessons()$lesson[1:6],
#'               modules = rep(c("day 1", "day 2"), each = 3))
#'
#' ### modules and course as dataframe
#' example_course <- lhLessons::example_course
#' head(example_course, 3)
#' #   module                        lesson
#' # 1  Day 1    r_programming_introduction
#' # 2  Day 1    r_quarto_literate_analysis
#' # 3  Day 1 activity_reproducibility_lego
#' setup_lessons(example_course)
#' }
#' @export

setup_lessons <- function(lessons, package = 'lhLessons', modules = NULL, overwrite = FALSE) {
  check_interactive()

  if(!verify_course_repo(query = 'Set up course lessons here? ')) return(invisible(NULL))

  ### Query lesson version (checks to ensure lessons package is installed!)
  v <- get_lessons_version(pkg = package, quiet = TRUE)
  message("Installing lessons from ", package, ", version ", v, "...")

  ### If lessons provided as data.frame, break into separate lesson and module vectors
  if(is.data.frame(lessons)) {
    if(is.null(modules) && 'module' %in% names(lessons)) modules <- lessons$module
    lessons <- lessons$lesson
  }
  ### If lessons provided as a named vector, break into separate lesson and module vectors
  ### because names are a fragile attribute...  An explicit `modules` argument
  ### takes precedence over names (as documented).
  if(!is.null(names(lessons))) {
    if(is.null(modules)) modules <- names(lessons)
    lessons <- unname(lessons)
  }
  if(!is.null(modules)) {
    if(length(modules) != length(lessons)) stop('Length of lessons and modules must match!')
  }

  ### strip qmd and rmd extensions from lesson vector
  lessons <- sub('\\.[qQrR]?md$', '', lessons)

  ### check that all lessons are in the lessons package (exact name match)
  lessons_available <- search_lessons(pkg = package)$lesson
  lessons_missing <- lessons[!lessons %in% lessons_available]
  if(length(lessons_missing) > 0) stop("Some lessons are not found in the ", package, " package:",
                                       paste0("\n\u25CF ", lessons_missing))

  ### if overwriting, remove the old lesson files and folders first (after
  ### confirming with the user); the old lesson list in _quarto.yml is reset
  ### inside setup_quarto_yml()
  if(overwrite) {
    ### check _quarto.yml can be reset *before* deleting anything
    yml_raw <- readr::read_file(here::here("_quarto.yml"))
    if(!grepl("# START SESSIONS", yml_raw, fixed = TRUE) ||
       !grepl("# END SESSIONS",   yml_raw, fixed = TRUE)) {
      stop("_quarto.yml has no '# START SESSIONS' / '# END SESSIONS' markers, ",
           "so the old lesson list can't be replaced; nothing was removed.")
    }
    if(!remove_lessons()) {
      message("Overwrite cancelled; nothing was changed.")
      return(invisible(NULL))
    }
  }

  ### set up _quarto.yml with links to sessions (other info set up in init_course())
  setup_quarto_yml(lessons, modules, overwrite)

  ### copy over lesson files from lessons package to current project: lessons, images, data, _extensions
  lessons_copied <- copy_lessons(lessons, from = "lessons", to = ".", pkg = package)

  ### copy over lesson-associated folders from lessons package to current project: lessons, images, data
  copy_folders(lessons, from = "lesson_images",    to = "images",    pkg = package)
  copy_folders(lessons, from = "lesson_data",      to = "data",      pkg = package)
  copy_folders(lessons, from = "lesson_slides",    to = "slides",    pkg = package)
  copy_folders(lessons, from = "lesson_resources", to = "resources", pkg = package)

  ### Set up and write out lessons metadata: module, lesson, lesson files, lesson repo, and repo version
  meta <- data.frame(module = if(is.null(modules)) NA_character_ else modules,
                     lesson = lessons) |>
    dplyr::left_join(lessons_copied, by = 'lesson') |>
    dplyr::mutate(package = package, pkg_version = v)
  ### drop duplicated rows (e.g., repeated lesson entries from the join), then
  ### sort by output filename; base R avoids a bare column name in R CMD check
  meta <- meta[!duplicated(meta), ]
  meta <- meta[order(basename(meta$f_out)), ]
  readr::write_csv(meta, here::here('metadata_lessons.csv'))

  message("Course populated with lessons!  Refresh the file pane to see the course files and folders.\n")

  message("To render the course book, restart RStudio to activate the Build tab.")

  check_git_steps()

  return(invisible(NULL))
}

### not exported!

remove_lessons <- function() {
  ### Remove installed lesson qmds (root-level sNN_<lesson>.qmd) and the
  ### matching lesson folders in images, data, slides, and resources.
  ### Lesson names come from the qmd filenames plus metadata_lessons.csv (if
  ### present).  Lists what will be deleted and asks for confirmation; returns
  ### TRUE if removal went ahead (or there was nothing to remove), else FALSE.
  qmd_fs <- list.files(here::here(), pattern = '^s[0-9]{2}_.+\\.qmd$')
  lessons_old <- sub('^s[0-9]{2}_(.+)\\.qmd$', '\\1', qmd_fs)

  meta_f <- here::here('metadata_lessons.csv')
  if(file.exists(meta_f)) {
    meta_old <- readr::read_csv(meta_f, show_col_types = FALSE)
    lessons_old <- union(lessons_old, meta_old$lesson)
  }

  folders <- c('images', 'data', 'slides', 'resources')
  ### relative paths (for display) and full paths (for deletion)
  dirs_rel <- file.path(rep(folders, each = length(lessons_old)), lessons_old)
  dirs_rel <- dirs_rel[dir.exists(file.path(here::here(), dirs_rel))]
  dirs_old <- file.path(here::here(), dirs_rel)

  if(length(qmd_fs) == 0 && length(dirs_old) == 0) {
    message("No existing lessons found to remove.")
    return(TRUE)
  }

  message("\u26A0\uFE0F WARNING: overwrite = TRUE is DESTRUCTIVE and cannot be undone. ",
          "Any local edits or customizations to these lessons will be permanently lost.",
          "\n  Commit your work to Git first, and consider running `compare_diffs()` to review ",
          "how your lessons differ from the lessons package.",
          "\nThe following existing lesson files and folders will be DELETED:",
          paste0("\n  \u2022 ", c(qmd_fs, dirs_rel)))
  ok <- readline("Delete these and rebuild the lessons? (y/n) ")
  if(tolower(ok) != 'y') return(FALSE)

  unlink(file.path(here::here(), qmd_fs))
  unlink(dirs_old, recursive = TRUE)
  message("Removed ", length(qmd_fs), " lesson file(s) and ", length(dirs_old), " lesson folder(s).")

  return(TRUE)
}

copy_lessons <- function(lessons, from, to = ".", pkg) {
  ### from is the directory to copy lessons from (inside the lessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames, without extensions

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)

  ### copy over files from lessons package to current project; must search
  ### the requested package, and the join keeps only exact lesson-name matches
  fs_avail <- search_lessons(pkg = pkg)

  fs_to_copy <- data.frame(lesson = lessons) |>
    dplyr::left_join(fs_avail, by = c("lesson"))
  ### base assignment (not mutate) avoids a bare column name that R CMD check flags
  fs_to_copy$f_out <- file.path(subfolder,
                                sprintf('s%02d_%s', seq_along(lessons), basename(fs_to_copy$lesson_file)))

  if(nrow(fs_to_copy) > 0) {
    copied <- file.copy(fs_to_copy$lesson_file, fs_to_copy$f_out)
    if(any(!copied)) {
      warning('Some lesson files failed to copy:',
              paste0('\n\u2022  ', basename(fs_to_copy$lesson_file[!copied])))
    }
  }

  return(fs_to_copy)
}

copy_folders <- function(lessons, from, to, pkg, lib.loc = NULL, overwrite = FALSE) {
  ### from is the directory to copy lessons from (inside the lessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames;
  ### lib.loc locates a non-default install of pkg (used by update_lessons())

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder, recursive = TRUE)

  lessons <- sub("\\.[qQrR]?md$", "", lessons) ### strip extension if necessary

  ### copy over folders and files from lessons package to current project
  fs_available <- list.files(system.file(from, package = pkg, lib.loc = lib.loc), full.names = TRUE)
  fs_to_copy <- fs_available[basename(fs_available) %in% lessons]

  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, subfolder, recursive = TRUE, overwrite = overwrite)
  }

}


define_lesson_txt <- function(lessons, modules) {

  if(is.null(modules)) {
    ### if lessons is not a named vector, simple case
    ### `lessons` vector is bare filenames, no extension - standardize to .qmd
    lesson_txt <- sprintf("      - s%02d_%s.qmd",
                           seq_along(lessons), lessons) |>
      paste(collapse = "\n")
  } else {
    message("Module names detected...")
    ### if lessons is a named vector, break into modules by name
    mod_names <- unique(modules) ### keep in order
    mod_vec <- mod_names
    names(mod_vec) <- mod_names

    ### number the lessons first; attach module names
    lsn_path <- sprintf('s%02d_%s', seq_along(lessons), lessons)

    ### loop over modules to create a unique set for each, with part and chapter
    for(mod in mod_names) {
      # mod <- mod_names[2]
      mod_lsns <- lsn_path[modules == mod]
      ### `lessons` vector is bare filenames, no extension - standardize to .qmd
      mod_lsn_vec <- sprintf("        - %s.qmd",
                             mod_lsns) |>
        paste(collapse = "\n")
      mod_txt <- sprintf("      - section: \"%s\"\n        contents:\n%s",
                         mod, mod_lsn_vec)
      mod_vec[mod] <- mod_txt
    }

    lesson_txt <- paste(mod_vec, collapse = "\n")
  }

  return(lesson_txt)
}

