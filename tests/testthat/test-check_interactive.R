### devtools::test() from the console is itself interactive, so 
### interactive() to FALSE to mimic a script or Rscript run

test_that("exported functions throw an error when run non-interactively", {
  withr::local_options(lhCore.allow_noninteractive = NULL)
  local_mocked_bindings(is_interactive = function() FALSE)

  expect_error(check_interactive(), "interactively")
  expect_error(setup_lessons("x"), "interactively")
  expect_error(init_course("x"), "interactively")
  expect_error(update_lessons(), "interactively")
  expect_error(checkin_lessons(), "interactively")
})

test_that("no error in an interactive session", {
  withr::local_options(lhCore.allow_noninteractive = NULL)
  local_mocked_bindings(is_interactive = function() TRUE)
  expect_true(check_interactive())
})

test_that("bypass option allows non-interactive runs (tests only)", {
  withr::local_options(lhCore.allow_noninteractive = TRUE)
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_true(check_interactive())
})
