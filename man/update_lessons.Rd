% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_lessons.R
\name{update_lessons}
\alias{update_lessons}
\title{Update lessons in a lhCore course}
\usage{
update_lessons(lessons = "all", pkg = "lhLessons", version = NULL)
}
\arguments{
\item{lessons}{A vector of specific lessons to be updated, based on file
names (bare or with \code{.qmd} extension).  If lesson names don't match
lessons in the source book, an error will be returned.  The default \code{all}
will update all available lessons, i.e., those whose lesson names match
those in the source book.  Customized lessons, whose names do not match
those in the source book, will not be updated, even with \code{version = 'all'}.}

\item{pkg}{Name of the package where lessons are curated, default "lhLessons"}

\item{version}{A version of the lhCore package from which the updated lessons
should be pulled.  \code{version} can be given as a commit hash, a tag, a
version, or a date.}
}
\description{
Update lessons in a lhCore course
}
\examples{
# update_lessons(lessons = c("git-github-intro", "r-creating-packages.qmd"),
#                version = "v0.0.1")
}
