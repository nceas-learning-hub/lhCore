#' Install a Quarto theme
#'
#' Install or update a desired Quarto theme to be applied to the course site.
#' Defaults to `nceas-learning-hub/lh_theme`.  This is called in `setup_lessons()`
#' but if the theme changes down the line, this can be used to update it by pulling
#' from an updated repo or a different repo.
#'
#' @param user If theme is in a user repository, provide a user name (default `NULL`).  If `NULL`,
#'     the organization name will be used.  If a `user` repository is desired, set
#'     `org` to `NULL` otherwise `org` will supercede `user`.
#' @param org If the theme is in an organization repository, provide the organization name.
#'     Default: `nceas-learning-hub`.  If a `user` repo is desired, set `org` to `NULL`.
#' @param repo The repository name where the Quarto extension can be found.  Default:
#'     Learning Hub theme `lh_theme`
#' @param theme The name of the theme within the `_extensions` folder in the repository.
#'     Defaults to the same name as the repo.
#'
#' @return If successful, returns the URL of the installed theme repository.
#' @export

install_theme <- function(user = NULL,
                          org = 'nceas-learning-hub',
                          repo = 'lh_theme',
                          theme = NULL) {
  ### If an org is provided, that supercedes a user.  For a user-specific theme,
  ### set org to NULL.

  ### NOTE: doing this the hard way because a simple `quarto add org/theme` has
  ### those annoying prompts that seem to get in the way.  If we can find a way
  ### to deal with those using system commands, that seems more robust!

  if(is.null(user)) {
    if(is.null(org)) stop('Must supply either a GitHub user or org!')
    top <- org
  } else {
    if(is.null(org)) top <- user
  }

  ### default theme name
  if(is.null(theme)) theme <- repo

  ### The strategy: clone into a temp folder, find the files, copy over, delete temp
  theme_url <- sprintf('https://github.com/%s/%s.git', top, repo)

  ### Set up temp repo location
  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  ### pre-clean up! (use an on.exit to make sure?)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  system(sprintf('git clone %s %s', theme_url, tmp_dir))

  ### identify _extensions and theme within it
  ext_dir <- list.files(tmp_dir, pattern = '_extensions',
                      include.dirs = TRUE, recursive = TRUE, full.names = TRUE)
  if(length(ext_dir) == 0) stop('No `_extensions` folder found!')

  theme_dir <- list.files(ext_dir, pattern = theme, include.dirs = TRUE, recursive = TRUE, full.names = TRUE)
  if(length(theme_dir) == 0) stop('No `', theme, '` folder found!')

  ### Prompt to make sure user trusts the source
  if(top != 'nceas-learning-hub') {
    message('Preparing to install theme from ', theme_url, '...')
    trust <- readline('? Do you trust the authors of this extension? (y/n) ')
    if(tolower(trust) != 'y') stop('aborting...')
  }

  ### First set up the directories in the project directory
  if(!file.exists(here::here('_extensions'))) dir.create(here::here('_extensions'))
  if(!file.exists(here::here('_extensions', top))) dir.create(here::here('_extensions', top))

  ### Copy over the files; if successful clean up and return the theme URL.
  success <- file.copy(theme_dir, here::here('_extensions', top), recursive = TRUE, overwrite = TRUE)
  if(!success) stop('Something went wrong copying the theme!')

  unlink(tmp_dir, recursive = TRUE)
  return(theme_url)

}
