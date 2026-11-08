test_that("versions are compared numerically, not as strings", {
  expect_equal(select_latest_tag(c("v0.9.0", "v0.10.0", "v0.2.1")), "v0.10.0")
})

test_that("date-style tags are handled, including unpadded components", {
  expect_equal(select_latest_tag(c("2024.09.15", "2024.10.01", "2023.12.31")), "2024.10.01")
  expect_equal(select_latest_tag(c("2024.9.15", "2024.10.1")), "2024.10.1")
})

test_that("the original tag string is returned, with or without a leading v", {
  expect_equal(select_latest_tag(c("v1.2.0", "1.1.0")), "v1.2.0")
  expect_equal(select_latest_tag(c("v1.2.0", "1.3.0")), "1.3.0")
})

test_that("non-version tags are ignored", {
  expect_equal(select_latest_tag(c("junk", "v0.1.0", "release-candidate")), "v0.1.0")
})

test_that("date tags outrank semantic-version tags when both exist", {
  expect_equal(select_latest_tag(c("v3.0.0", "2024.01.01")), "2024.01.01")
})

test_that("NA is returned when no tag looks like a version", {
  expect_identical(select_latest_tag(c("junk", "other")), NA_character_)
  expect_identical(select_latest_tag(character(0)), NA_character_)
})
