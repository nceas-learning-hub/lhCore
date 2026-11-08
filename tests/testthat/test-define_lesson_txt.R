test_that("without modules, lessons are numbered in order as a flat list", {
  expect_equal(
    define_lesson_txt(c("a", "b"), modules = NULL),
    "      - s01_a.qmd\n      - s02_b.qmd"
  )
})

test_that("with modules, lessons are grouped into sections", {
  txt <- suppressMessages(define_lesson_txt(c("a", "b", "c"), modules = c("Day 1", "Day 1", "Day 2")))

  expect_equal(
    txt,
    paste0('      - section: "Day 1"\n        contents:\n        - s01_a.qmd\n        - s02_b.qmd\n',
           '      - section: "Day 2"\n        contents:\n        - s03_c.qmd')
  )
})

test_that("modules keep first-appearance order, and lesson numbers follow input order", {
  txt <- suppressMessages(define_lesson_txt(c("a", "b", "c"), modules = c("B", "A", "B")))

  expect_equal(
    txt,
    paste0('      - section: "B"\n        contents:\n        - s01_a.qmd\n        - s03_c.qmd\n',
           '      - section: "A"\n        contents:\n        - s02_b.qmd')
  )
})

test_that("the module message is shown when modules are given", {
  expect_message(define_lesson_txt(c("a", "b"), modules = c("x", "y")), "Module names detected")
})
