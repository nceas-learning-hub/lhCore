### Builds a throwaway course folder; here::here() is pointed at it and
### readline() is answered programmatically, so nothing real is touched.

local_fake_course <- function(env = parent.frame()) {
  course <- withr::local_tempdir(.local_envir = env)
  ### lesson files and folders for two installed lessons, plus things to keep
  file.create(file.path(course, c("s01_lesson_a.qmd", "s02_lesson_b.qmd",
                                  "index.qmd", "notes.qmd")))
  for(f in c("images", "data", "slides", "resources")) {
    dir.create(file.path(course, f, "lesson_a"), recursive = TRUE)
    dir.create(file.path(course, f, "lesson_b"))
    dir.create(file.path(course, f, "keep_me"))
  }
  writeLines(c("    contents:",
               "      - index.qmd",
               "      # START SESSIONS",
               "      - s01_lesson_a.qmd",
               "      - s02_lesson_b.qmd",
               "      # END SESSIONS",
               "",
               "  page-footer:"),
             file.path(course, "_quarto.yml"))
  testthat::local_mocked_bindings(here = function(...) file.path(course, ...),
                                  .package = "here", .env = env)
  course
}

test_that("remove_lessons() deletes lesson qmds and folders after confirmation", {
  course <- local_fake_course()
  testthat::local_mocked_bindings(readline = function(...) "y", .package = "base")

  expect_message(res <- remove_lessons(), "Removed 2 lesson file")
  expect_true(res)

  expect_false(any(file.exists(file.path(course, c("s01_lesson_a.qmd", "s02_lesson_b.qmd")))))
  expect_true(all(file.exists(file.path(course, c("index.qmd", "notes.qmd")))))
  for(f in c("images", "data", "slides", "resources")) {
    expect_false(dir.exists(file.path(course, f, "lesson_a")))
    expect_false(dir.exists(file.path(course, f, "lesson_b")))
    expect_true(dir.exists(file.path(course, f, "keep_me")))
  }
})

test_that("remove_lessons() deletes nothing when the user declines", {
  course <- local_fake_course()
  testthat::local_mocked_bindings(readline = function(...) "n", .package = "base")

  suppressMessages(res <- remove_lessons())
  expect_false(res)
  expect_true(file.exists(file.path(course, "s01_lesson_a.qmd")))
  expect_true(dir.exists(file.path(course, "images", "lesson_a")))
})

test_that("remove_lessons() also finds folders listed in metadata_lessons.csv", {
  course <- local_fake_course()
  ### lesson_c has a folder but no qmd; only the metadata knows about it
  dir.create(file.path(course, "data", "lesson_c"))
  writeLines(c("module,lesson", "NA,lesson_c"), file.path(course, "metadata_lessons.csv"))
  testthat::local_mocked_bindings(readline = function(...) "y", .package = "base")

  suppressMessages(remove_lessons())
  expect_false(dir.exists(file.path(course, "data", "lesson_c")))
})

test_that("remove_lessons() reports when there is nothing to remove", {
  course <- withr::local_tempdir()
  testthat::local_mocked_bindings(here = function(...) file.path(course, ...),
                                  .package = "here")
  expect_message(res <- remove_lessons(), "No existing lessons")
  expect_true(res)
})

test_that("setup_quarto_yml(overwrite = TRUE) replaces the old lesson list", {
  course <- local_fake_course()

  suppressMessages(setup_quarto_yml(c("lesson_c", "lesson_d"), modules = NULL, overwrite = TRUE))
  yml <- readLines(file.path(course, "_quarto.yml"))

  expect_false(any(grepl("lesson_a|lesson_b", yml)))
  expect_true(any(grepl("s01_lesson_c.qmd", yml, fixed = TRUE)))
  expect_true(any(grepl("s02_lesson_d.qmd", yml, fixed = TRUE)))
  ### markers and surrounding content survive, so it can be overwritten again
  expect_true(any(grepl("# START SESSIONS", yml, fixed = TRUE)))
  expect_true(any(grepl("# END SESSIONS", yml, fixed = TRUE)))
  expect_true(any(grepl("index.qmd", yml, fixed = TRUE)))
  expect_true(any(grepl("page-footer:", yml, fixed = TRUE)))
})

test_that("setup_quarto_yml(overwrite = TRUE) works repeatedly, with modules", {
  course <- local_fake_course()

  suppressMessages(setup_quarto_yml(c("lesson_c", "lesson_d"), modules = c("Day 1", "Day 2"), overwrite = TRUE))
  suppressMessages(setup_quarto_yml(c("lesson_e"), modules = NULL, overwrite = TRUE))
  yml <- readLines(file.path(course, "_quarto.yml"))

  expect_false(any(grepl("lesson_c|lesson_d|Day 1", yml)))
  expect_equal(sum(grepl("s01_lesson_e.qmd", yml, fixed = TRUE)), 1)
  expect_equal(sum(grepl("# START SESSIONS", yml, fixed = TRUE)), 1)
})

test_that("setup_quarto_yml(overwrite = TRUE) errors without markers, and leaves the file alone", {
  course <- local_fake_course()
  writeLines(c("contents:", "  - index.qmd"), file.path(course, "_quarto.yml"))

  expect_error(setup_quarto_yml("lesson_c", NULL, overwrite = TRUE), "markers")
  expect_equal(readLines(file.path(course, "_quarto.yml")), c("contents:", "  - index.qmd"))
})
