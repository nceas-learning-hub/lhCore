### Build the `test_course` example dataset: a one-row data frame of course info
### that can be passed as `course_info` to init_course().  Illustrative values only.
### Output: data/test_course.rda (via usethis::use_data)
### Run from the package root.

test_course <- data.frame(
  course_proj  = "test_course",
  course_org   = "nceas-learning-hub",
  course_title = "CoreR",
  course_desc  = "CoreR course offered at NCEAS in Oct 2024",
  course_dates = "October 2024",
  setup_github = "TRUE",
  template     = "corer",
  package      = "lhLessons",
  loc          = ".",
  quiet        = "TRUE")

usethis::use_data(test_course, overwrite = TRUE)
