### resolve_lessons() looks for local lessons via here::here(), so point
### here() at a temp "course" folder containing a few fake lesson files.

local_course <- function(env = parent.frame()) {
  course <- withr::local_tempdir(.local_envir = env)
  file.create(file.path(course, c("s01_a.qmd", "s02_b.qmd", "s03_c.qmd", "other.txt")))
  testthat::local_mocked_bindings(here = function(...) course, .package = "here",
                                  .env = env)
  course
}

test_that("NULL finds all prefixed lesson files and builds remote/name columns", {
  course <- local_course()
  res <- resolve_lessons(NULL)

  expect_equal(basename(res$local), c("s01_a.qmd", "s02_b.qmd", "s03_c.qmd"))
  expect_equal(res$remote, c("a.qmd", "b.qmd", "c.qmd"))
  expect_equal(res$name, c("a", "b", "c"))
})

test_that("character input matches local files by name", {
  local_course()
  res <- resolve_lessons("b")

  expect_equal(basename(res$local), "s02_b.qmd")
  expect_equal(res$name, "b")
})

test_that("character input warns when fewer lessons are found than requested", {
  local_course()
  expect_warning(resolve_lessons(c("a", "zzz")), "2 lessons requested, 1 lessons found")
})

test_that("compare_diffs() output keeps only changed lessons", {
  course <- local_course()
  diffs <- data.frame(file_remote = c("r1", "r2", NA),
                      file_local  = file.path(course, c("s01_a.qmd", "s02_b.qmd", "s03_c.qmd")),
                      result      = c("same", "changed", "New file detected"),
                      status      = c(0, 1, 2))
  res <- resolve_lessons(diffs)

  expect_equal(res$name, c("b", "c"))
})

test_that("data frames missing required columns are rejected", {
  local_course()
  expect_error(resolve_lessons(data.frame(lesson = "a")), "output from compare_diffs")
})

test_that("unsupported input types are rejected", {
  local_course()
  expect_error(resolve_lessons(42), "must be NULL, a character vector")
})
