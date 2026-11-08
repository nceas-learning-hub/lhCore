test_that("the default branch is read from the HEAD symref line", {
  lines <- c("ref: refs/heads/main\tHEAD",
             "011c6c3958b540e4977cac7dca7ca00af86f7834\tHEAD")
  expect_equal(parse_default_branch(lines), "main")
})

test_that("non-main default branches and slashes in branch names are handled", {
  expect_equal(parse_default_branch(c("ref: refs/heads/master\tHEAD", "abc\tHEAD")), "master")
  expect_equal(parse_default_branch("ref: refs/heads/release/v2\tHEAD"), "release/v2")
})

test_that("NA is returned when the remote reports no symref", {
  expect_identical(parse_default_branch("011c6c3958b540e4977cac7dca7ca00af86f7834\tHEAD"),
                   NA_character_)
  expect_identical(parse_default_branch(character(0)), NA_character_)
})
