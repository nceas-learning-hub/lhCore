% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/available_lessons.R
\name{available_lessons}
\alias{available_lessons}
\title{Get a dataframe of available lessons}
\usage{
available_lessons(lessons = NULL, pkg = "lhLessons", quiet = TRUE)
}
\arguments{
\item{lessons}{An optional character vector of lessons to query whether
specific lessons are available}

\item{pkg}{The package to query for lesson availability (default \code{lhLessons})}

\item{quiet}{Suppress messages?}
}
\value{
A data frame containing the file names (.qmd) of all (or selected)
lessons from the lhLessons package.  Eventually, including file title, date, and
author (if available in the .qmd)
}
\description{
Get a dataframe of available lessons
}
\examples{
\dontrun{available_lessons(lessons = c('r_functions.qmd', 'this_package_does_not_exist.Rmd'))}
}
