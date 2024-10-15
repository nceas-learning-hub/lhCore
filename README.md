
<!-- README.md is generated from README.Rmd. Please edit that file -->

# coreR

<!-- badges: start -->
<!-- badges: end -->

The goal of coreR is to …

## Installation

You can install the development version of coreR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nceas-learning-hub/coreR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(coreR)
## basic example code
```

## Notes for development

Quarto documents can go into a folder called `inst/quarto` or
`inst/sections` or the like. This `inst` folder can have any structure
or files inside, but the next level (i.e., `quarto` or `sections`)
cannot conflict with any folders in the base level of the package (e.g.,
`R`, `man`, etc).

- These can be accessed using
  `system.file('sections', '<filename>', package = 'coreR')` and then
  copied into a local file structure.

We can use
`usethis::create_project(path = "MyNewProject", open = TRUE, rstudio = TRUE)`
to create a new bare R project, and then populate with the file
structure desired

- fixed structure and files can be copied from `inst/`
- workshop-specific files (e.g., anything that has the workshop name in
  it) can be either copied & modified or created from whole cloth

How to tag the workshop with some info about the version of package used
to build it? metadata in the qmds or yaml header?

# From NCEAS/nceas-training (update!):

This repository contains lessons used in NCEAS training events. The
lessons are all written in RMarkdown and set up so that they build as a
bookdown.

To contribute, see [our contributing document](contributing.md)

## Customizing Materials

To create a custom book for a specific training, create a new branch for
the training event (eg 2019-11-RRCourse). In that branch, you can make
changes to \_bookdown.yml to specify which content to include, and you
can modify chapters. The built book should be hosted on another
repository specific to that training event, **not** this repository.
Please do not commit built versions of the book. Additionally, when
adding material please carefully consider file size. PDF presentations
should be compressed, and data files, if absolutely necessary, should be
small (\< 1MB).

## Updating Materials

Changes to chapters that would be beneficial to other training events
should be merged back into the master branch.
