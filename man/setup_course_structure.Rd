% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_course_structure.R
\name{setup_course_structure}
\alias{setup_course_structure}
\title{Set up the basic structure of a course}
\usage{
setup_course_structure(
  template = c("lh", "adc", "delta", "corer")[1],
  package = "lhLessons",
  overwrite = FALSE
)
}
\arguments{
\item{template}{The name of the course template to use.  This should be a character
length 1 object.  Default template will be "lh" ("Learning Hub") but other templates may
include "adc", "delta", "lter", "corer", etc.  Template affects the appearance of the
course book (scss, logos), and the default index boilerplate.}

\item{package}{The name of the course lessons package to use.}

\item{overwrite}{If there is already an existing course structure, and
\code{overwrite = FALSE}, will return an error.  If the user wishes to create
a new lesson structure, then \code{overwrite = TRUE} will remove the existing
structure (including lessons) in its entirety and create a new structure from scratch.}
}
\description{
This function follows from init_course() to set up the basic file structure of
a new course.  Sets up the _quarto.yml, theme files, and an index.qmd file
based on the user's specification of a course template.  These files are pulled
from the \code{lhLessons} package.
}
\examples{
\dontrun{
  setup_course_structure(template = 'adc')
}
}
