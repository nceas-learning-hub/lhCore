#' Install a Quarto theme
#'
#' Install or update a desired Quarto theme to be applied to the course site.
#' Defaults to `nceas-learning-hub/lh_theme`.  This is called in `setup_lessons()`
#' but if the theme changes down the line, this can be used to update it by pulling
#' from an updated repo or a different repo.
#'
#' Note this can also be done manually in the Terminal, using `quarto add <org name>/<theme extension repo>`.
#'
#' @param org Organization repository where theme extension is located.
#'     Default: `nceas-learning-hub`.  Extension repo is called `theme_X` where `X` is
#'     one of the available themes.
#' @param theme The name of the theme within the `_extensions` folder in the repository.
#'     Currently, one of `lh`, `adc`, `delta`, `corer`.
#'
#' @return If successful, returns the extension location (org/theme repo)
#' @export

install_theme <- function(org = 'nceas-learning-hub',
                          theme = c('lh', 'adc', 'delta', 'corer')[1]) {

  extension_dir <- sprintf('%s/theme_%s', org, theme)

  quarto_add <- sprintf('quarto add %s --no-prompt', extension_dir)

  system(quarto_add)

  return(extension_dir)
}
