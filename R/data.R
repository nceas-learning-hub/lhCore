#' Example course information for `init_course()`
#'
#' A one-row data frame of illustrative course information, in the format
#' accepted as `course_info` by [init_course()].  Column names match the arguments of
#' `init_course()`.  Values are examples only.  Note that `setup_github = "TRUE"` will create a
#' Github repository, so change it to `"FALSE"` to test locally.
#'
#' @format A data frame with 1 row and 10 character columns:
#' \describe{
#'   \item{course_proj}{Project name, `"test_course"`}
#'   \item{course_org}{Github organization}
#'   \item{course_title}{Course title}
#'   \item{course_desc}{Course description}
#'   \item{course_dates}{Course dates, `"October 2024"`}
#'   \item{setup_github}{Set up Git/Github? (text, `"TRUE"`)}
#'   \item{template}{Style template}
#'   \item{package}{Lessons package}
#'   \item{loc}{Location for the new repository, `"."`}
#'   \item{quiet}{Suppress messages? (text, `"TRUE"`)}
#' }
#' @source Built by `data-raw/test_course.R`.
#' @examples
#' data(test_course)
#' test_course
"test_course"
