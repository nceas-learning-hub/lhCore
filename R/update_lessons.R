#' Update lessons in a coreR course
#'
#' @param lessons A vector of specific lessons to be updated, based on file
#'     names (bare or with `.qmd` extension).  If lesson names don't match
#'     lessons in the source book, an error will be returned.  The default `all`
#'     will update all available lessons, i.e., those whose lesson names match
#'     those in the source book.  Customized lessons, whose names do not match
#'     those in the source book, will not be updated, even with `version = 'all'`.
#' @param pkg Name of the package where lessons are curated, default "coreRlessons"
#' @param version A version of the coreR package from which the updated lessons
#'     should be pulled.  `version` can be given as a commit hash, a tag, a
#'     version, or a date.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # update_lessons(lessons = c("git-github-intro", "r-creating-packages.qmd"),
#' #                version = "v0.0.1")

update_lessons <- function(lessons = "all", pkg = "coreRlessons", version = NULL) {
  ### placeholder - give a vector of lesson names, optionally give a version tag
  ### if version tag is NULL use latest otherwise use whatever the user supplied
  ### in the `@ref` portion - could be a commit, tag, branch name
  ### install_github("nceas-learning-hub", "coreRlessons@v0.1.1")

  ### if version is not current, install coreRlessons from requested version
  ### identify lessons to be updated - which match the names in coreRlessons at this version
  ### copy over into sections folder, overwriting old versions

  installed_version <- get_lessons_version(pkg, quiet = TRUE)
  if(installed_version == version) {
    stop("Requested version (", version, ") is identical to installed version (", installed_version, ")...")
  }

  #####################################################
  ### Set up temporary package with desired version ###
  #####################################################

  ### Set up temp library location for desired version
  tmp_lib <- tempfile()
  dir.create(tmp_lib)

  ### pre-clean up! (use an on.exit to make sure?)
  on.exit(unlink(tmp_lib, recursive = TRUE))

  ### Access the lessons pkg on Github and install in temp library location
  message('Installing package ', pkg, ' version ', version, ' into temp library...')
  withr::with_libpaths(tmp_lib, {
    install.packages('remotes') ### b/c the next line access `remotes` from tmp_lib
    remotes::install_github(sprintf("nceas-learning-hub/%s", pkg),
                            ref = version)
  })

  #################################
  ### Identify desired lessons ###
  #################################

  ### Identify lessons in current course
  lessons_current <- list.files(here::here(), pattern = "s[0-9]{2}_.+qmd$")
  lessons_available <- list.files(system.file("lessons", package = pkg), full.names = TRUE)

  l_c_df <- data.frame(current_f = lessons_current)
  l_c_df$lesson <- gsub("s[0-9]{2}_|.qmd$", "", basename(lessons_current))
  l_c_df$prefix <- regmatches(lessons_current, m = regexpr("^s[0-9]{2}", lessons_current))

  l_a_df <- data.frame(avail_f = lessons_available)
  l_a_df$lesson <- gsub("s[0-9]{2}_|.qmd$", "", basename(lessons_available))

  l_join_df <- dplyr::inner_join(l_c_df, l_a_df, by = "lesson")
  if(all(lessons != "all")) {
    ### if any are "all" just update all; if none are "all" then update selected
    ### strip extension just in case
    lessons <- gsub(".qmd", "", lessons)
    ### filter to just those given lessons
    l_join_df <- l_join_df[l_join_df$lesson %in% lessons, ]
  }

  ############################################
  ### Report matched lessons and copy over ###
  ############################################
  if(nrow(l_join_df) == 0) stop("No valid lessons identified for updating!")
  message("Updating lessons to version ", version, ":\n  \u2022 ",
          paste(l_join_df$lesson, collapse = "\n  \u2022 "))

  proceed <- readline("Do you wish to update these files? (y/n) ")
  if(tolower(proceed) != "y") {
    unlink(tmp_lib, recursive = TRUE)
    stop("Update canceled!")
  }

  ### update files
  file.copy(from = l_join_df$avail_f, to = here::here(l_join_df$current_f), overwrite = TRUE)

  ### update folders
  update_folder(l_join_df$lesson, from = "lesson_images", to = "images", tmp_lib = tmp_lib)
  update_folder(l_join_df$lesson, from = "lesson_data",   to = "data", tmp_lib = tmp_lib)
  update_folder(l_join_df$lesson, from = "lesson_slides", to = "slides", tmp_lib = tmp_lib)

  # update _quarto.yml with version flags on updated lines
  yml <- readr::read_file(here::here('_quarto.yml'))
  for(i in 1:nrow(l_join_df)) {
    updated_f <- l_join_df$current_f[i]
    old_line <- sprintf("%s  ### .+?\\n", updated_f)
    new_line <- sprintf("%s  ###  (%s version %s)\n", updated_f, pkg, version)
    yml <- sub(old_line, new_line, yml)
  }

  readr::write_file(yml, here::here('_quarto.yml'))
  unlink(tmp_lib, recursive = TRUE)

}


### support functions

update_folder <- function(lessons, from, to, tmp_lib) {
  ### from is the directory to copy lessons from (inside the coreRlessons package);
  ### to is the directory to copy the lessons to (inside the course repository)
  ### lessons is the list of lesson filenames;

  ### create subfolder as needed
  subfolder <- here::here(to)
  if(!dir.exists(subfolder)) dir.create(subfolder)

  ### copy over folders and files from coreRlessons to current project
  fs_available <- list.files(system.file(from, package = "coreRlessons", lib.loc = tmp_lib), full.names = TRUE)
  fs_to_copy <- fs_available[basename(fs_available) %in% lessons]

  if(length(fs_to_copy) > 0) {
    file.copy(fs_to_copy, subfolder, recursive = TRUE)
  }

}
