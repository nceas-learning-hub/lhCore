### git_diff() shells out to `git diff --no-index`, so skip if git is missing.

write_pair <- function(remote_txt, local_txt, env = parent.frame()) {
  ### directory name includes a space to check that paths are quoted
  dir <- file.path(withr::local_tempdir(.local_envir = env), "with space")
  dir.create(dir)
  remote <- file.path(dir, "remote.qmd")
  local  <- file.path(dir, "local.qmd")
  writeLines(remote_txt, remote)
  writeLines(local_txt, local)
  list(remote = remote, local = local)
}

test_that("identical files give status 0 and a 'no difference' result", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  f <- write_pair("same line", "same line")
  res <- git_diff(f$remote, f$local)

  expect_equal(res$status, 0)
  expect_equal(res$result, "No difference detected")
  expect_named(res, c("file_remote", "file_local", "result", "status"))
})

test_that("different files give status 1 and the diff text", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  f <- write_pair("old line", "new line")
  res <- git_diff(f$remote, f$local)

  expect_equal(res$status, 1)
  expect_match(res$result, "-old line", fixed = TRUE)
  expect_match(res$result, "+new line", fixed = TRUE)
  expect_equal(res$file_local, f$local)
})

test_that("git warnings on stderr aren't mistaken for differences", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  ### With core.autocrlf = true, git prints "LF will be replaced by CRLF" to stderr for
  ### LF files (on Windows this lands in the captured output). Use a temporary global
  ### config (needs git >= 2.32 to be honored; otherwise the test passes trivially)
  dir <- withr::local_tempdir()
  cfg <- file.path(dir, "gitconfig")
  writeLines(c("[core]", "  autocrlf = true"), cfg)
  withr::local_envvar(GIT_CONFIG_GLOBAL = cfg)
  remote <- file.path(dir, "remote.qmd")
  local  <- file.path(dir, "local.qmd")
  ### writeBin so the files really have LF endings, even on Windows
  writeBin(charToRaw("same line\n"), remote)
  writeBin(charToRaw("same line\n"), local)
  res <- git_diff(remote, local)

  expect_equal(res$status, 0)
  expect_equal(res$result, "No difference detected")
})

test_that("a missing file is an error, not reported as a difference", {
  f <- write_pair("same line", "same line")
  expect_error(git_diff(f$remote, file.path(dirname(f$local), "does_not_exist.qmd")),
               "file not found")
})

test_that("word_diff = TRUE produces word-level markup", {
  skip_if(!nzchar(Sys.which("git")), "git not available")
  f <- write_pair("one two three", "one 2 three")
  res <- git_diff(f$remote, f$local, word_diff = TRUE)

  expect_equal(res$status, 1)
  expect_match(res$result, "[-two-]", fixed = TRUE)
})
