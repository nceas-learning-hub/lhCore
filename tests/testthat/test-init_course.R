### The location check runs before anything is created, so these tests only
### exercise the failure path and never create a project or touch GitHub.

test_that("init_course() stops when loc is inside a Git repo", {
  skip_if_not_installed("gert")

  repo <- withr::local_tempdir()
  gert::git_init(repo)

  expect_error(init_course("test_course", loc = repo),
               "inside the Git repository")
  expect_false(dir.exists(file.path(repo, "test_course")))
})

test_that("init_course() detects a Git repo above a loc that doesn't exist yet", {
  skip_if_not_installed("gert")

  repo <- withr::local_tempdir()
  gert::git_init(repo)
  new_loc <- file.path(repo, "not", "yet", "there")

  expect_error(init_course("test_course", loc = new_loc),
               "inside the Git repository")
  expect_false(dir.exists(file.path(repo, "not")))
})

test_that("init_course() rejects an unknown template before doing anything", {
  expect_error(init_course("test_course", template = "nope"), "should be one of")
})
