### The location check runs before anything is created, so these tests only
### exercise the failure path and never create a project or touch GitHub.

test_that("init_course() stops when loc is inside a Git repo", {
  skip_if_not_installed("gert")

  repo <- withr::local_tempdir()
  gert::git_init(repo)

  expect_error(init_course(course_proj = "test_course", loc = repo),
               "inside the Git repository")
  expect_false(dir.exists(file.path(repo, "test_course")))
})

test_that("init_course() detects a Git repo above a loc that doesn't exist yet", {
  skip_if_not_installed("gert")

  repo <- withr::local_tempdir()
  gert::git_init(repo)
  new_loc <- file.path(repo, "not", "yet", "there")

  expect_error(init_course(course_proj = "test_course", loc = new_loc),
               "inside the Git repository")
  expect_false(dir.exists(file.path(repo, "not")))
})

test_that("init_course() rejects an unknown template before doing anything", {
  expect_error(init_course(course_proj = "test_course", template = "nope"), "should be one of")
})

### course info supplied through `course_info` (data frame or named list)

test_that("parse_init_args() accepts a one-row data frame and drops NA/blank values", {
  df <- data.frame(course_proj = "my_course", course_title = "A title",
                   course_dates = NA_character_, setup_github = "FALSE",
                   template = "corer", stringsAsFactors = FALSE)
  out <- parse_init_args(df)
  expect_equal(out$course_proj, "my_course")
  expect_equal(out$template, "corer")
  expect_false(out$setup_github)
  expect_false("course_dates" %in% names(out))
  expect_false("course_org" %in% names(out))
})

test_that("parse_init_args() accepts a named list and coerces logicals", {
  out <- parse_init_args(list(course_proj = "my_course", quiet = "TRUE", loc = NULL))
  expect_equal(names(out), c("course_proj", "quiet"))
  expect_true(out$quiet)
})

test_that("the test_course dataset passes parse_init_args()", {
  data("test_course", package = "lhCore", envir = environment())
  out <- parse_init_args(test_course)
  expect_setequal(names(out), names(test_course))
  expect_equal(out$course_proj, "test_course")
  expect_equal(out$course_title, "CoreR")
  expect_equal(out$course_dates, "October 2024")
  expect_equal(out$loc, ".")
  expect_true(out$setup_github)
  expect_true(out$quiet)
})

test_that("parse_init_args() rejects bad input", {
  expect_error(parse_init_args(data.frame(course_proj = c("a", "b"))), "exactly one row")
  expect_error(parse_init_args(list(course_proj = "a", titel = "typo")), "Unrecognized")
  expect_error(parse_init_args(list("a")), "must have names")
  expect_error(parse_init_args("a"), "data frame or a named list")
  expect_error(parse_init_args(list(course_proj = c("a", "b"))), "single value")
  expect_error(parse_init_args(list(course_proj = "a", quiet = "maybe")), "TRUE or FALSE")
})

test_that("init_course() reads its arguments from `course_info`", {
  skip_if_not_installed("gert")

  repo <- withr::local_tempdir()
  gert::git_init(repo)

  ### reaches the location check, so `loc` and `course_proj` were picked up from course_info
  expect_error(init_course(course_info = list(course_proj = "test_course", loc = repo)),
               "inside the Git repository")
  expect_error(init_course(course_info = data.frame(course_proj = "test_course", loc = repo)),
               "inside the Git repository")
  ### template from course_info is validated
  expect_error(init_course(course_info = list(course_proj = "test_course", template = "nope")),
               "should be one of")
})

test_that("init_course() errors on `course_info` combined with arguments, or without course_proj", {
  expect_error(init_course(course_proj = "a", course_info = list(course_proj = "b")), "not both")
  expect_error(init_course(course_info = list(course_title = "no project name")), "course_proj")
  expect_error(init_course(), "course_proj")
})
