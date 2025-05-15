init_quarto_yml <- function(lessons, package, overwrite = FALSE) {

  qmd_yml_f_pkg <- list.files(system.file("course_files", package = package),
                              pattern = '_quarto_template.yml',
                              full.names = TRUE)
  if(length(qmd_yml_f_pkg) == 0) stop("No _quarto_template.yml found in package: ", package)
  qmd_yml_f_lcl <- here::here("_quarto.yml")

  ### copy package file to local file
  file.copy(qmd_yml_f_pkg, qmd_yml_f_lcl, overwrite = overwrite)

  ### get metadata for fields
  meta <- get_course_metadata()

  course_repo <- sprintf('https://github.com/%s/%s', meta["course_org"], meta["course_repo"])
  course_url  <- sprintf('%s.github.io/%s', meta["course_org"], meta["course_repo"])

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- readr::read_file(qmd_yml_f_lcl) |>
    stringr::str_replace_all("COURSE_REPO", course_repo) |>
    stringr::str_replace_all("COURSE_URL", course_url) |>
    stringr::str_replace_all("COURSE_TITLE", meta["course_title"]) |>
    stringr::str_replace_all("COURSE_DATES", meta["course_dates"])

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, qmd_yml_f_lcl)
}


setup_quarto_yml <- function(lessons, modules, prefix = "s", overwrite = FALSE) {
  quarto_yml_file <- here::here("_quarto.yml")

  ### Check that file exists, and check that SESSION_LINKS are present
  if(!file.exists(quarto_yml_file)) stop("No _quarto.yml file found in current directory...")

  quarto_yml_raw <- readr::read_file(quarto_yml_file)

  if(!overwrite & !stringr::str_detect(quarto_yml_raw, "SESSION_LINKS")) {
    stop("_quarto.yml: No fields for SESSION_LINKS, and overwrite is FALSE - _quarto.yml file not updated")
  }

  ### define lesson list
  meta <- get_course_metadata() ### get metadata for fields
  lesson_txt <- define_lesson_txt(lessons, modules, prefix)

  ### Update the _quarto.yml with all the good info!
  quarto_yml_txt <- quarto_yml_raw |>
    stringr::str_replace_all(" *# SESSION_LINKS", lesson_txt)

  ### write out updated yml file
  readr::write_file(quarto_yml_txt, quarto_yml_file)
}
