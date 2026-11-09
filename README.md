
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lhCore

<!-- badges: start -->

<!-- badges: end -->

`lhCore` builds a Quarto course website from a library of lessons. The
lessons live in a separate lessons package (by default
[`lhLessons`](https://github.com/nceas-learning-hub/lhLessons));
`lhCore` provides the functions to set up a course repository, choose
and order lessons, keep them in sync with the lessons package, and check
changes back in.

For full documentation, guides, and details on the `lhCore` and
`lhLessons` packages, see the **lh_packages website**:
<https://nceas-learning-hub.github.io/lh_packages/>.

## Installation

Install the development version of `lhCore` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("nceas-learning-hub/lhCore")
```

You will also need the lessons package, [Quarto](https://quarto.org/) on
your `PATH`, and Git/GitHub credentials configured if you want
`init_course()` to create and publish a GitHub repository.

``` r
pak::pak("nceas-learning-hub/lhLessons")
```

## Workflow

Functions that create or change course files (`init_course()`,
`setup_lessons()`, `update_lessons()`, `checkin_lessons()`) are meant to
be run by hand in an interactive session, not from a script. They stop
with an error otherwise.

``` r
library(lhCore)

### 1. Create the course project, metadata file, and (optionally) the GitHub repo
init_course(course_proj  = "2026-11-coreR",
            course_title = "coreR November 2026",
            course_desc  = "coreR course offered at NCEAS in Nov 2026",
            course_dates = "Nov. 2 - 6, 2026",
            template     = "corer")

### 2. Browse the lessons available in the lessons package
search_lessons()

### 3. Install the chosen lessons (in order, optionally grouped into modules)
setup_lessons(lessons = lhLessons::example_course)
```

| Function | Purpose |
|:---|:---|
| `init_course()` | Create a course project with `_quarto.yml`, `index.qmd`, theme, `metadata_course.csv`, and optionally the GitHub repo and publishing action. |
| `search_lessons()` | List and search the lessons (and their tags) in the lessons package. |
| `setup_lessons()` | Copy chosen lessons and their images, data, slides, and resources into the course; write the lesson list to `_quarto.yml` and `metadata_lessons.csv`. |
| `compare_diffs()` | Compare local lessons against a fresh install of the lessons package (latest, a branch, or a tag/commit); results go to `git_diff.txt`. |
| `update_lessons()` | Overwrite local lessons with a requested version from the lessons package. |
| `checkin_lessons()` | Check modified or new lessons back in to the lessons repository. |
| `query_lessons_tags()`, `get_lessons_version()` | Look up tags of the lessons repository and the installed lessons version. |

### Templates

`init_course(template = )` sets the landing page, banner, and logo:
`"lh"` (Learning Hub, default), `"adc"` (Arctic Data Center), `"delta"`
(Delta Stewardship Council), or `"corer"` (coreR).

## Caution: destructive functions

`setup_lessons(overwrite = TRUE)` and `update_lessons()` permanently
replace lesson files and folders in your course, discarding any local
edits. Commit your work to Git first, and run `compare_diffs()` to
review how your lessons differ from the lessons package before
overwriting.

- `setup_lessons(overwrite = TRUE)` deletes the existing lesson qmds
  (`sNN_*.qmd`) and matching lesson folders in `images`, `data`,
  `slides`, and `resources`, and rebuilds the lesson list between the
  `# START SESSIONS` and `# END SESSIONS` markers in `_quarto.yml`.
- `update_lessons()` overwrites only lessons whose names match the
  requested lessons package version; customized lessons with unmatched
  names are left alone.

## Course project layout

After `init_course()` and `setup_lessons()`, a course contains:

| Path | Contents |
|:---|:---|
| `_quarto.yml` | Site configuration and lesson list. |
| `index.qmd` | Landing page (edit to describe the course). |
| `sNN_<lesson>.qmd` | Lesson files, numbered in course order. |
| `images/`, `data/`, `slides/`, `resources/` | One subfolder per lesson. |
| `metadata_course.csv` | Course name, organization, title, description, dates. |
| `metadata_lessons.csv` | Module, lesson, source files, and the lessons package name and version each lesson came from. |

## Citation

``` r
citation("lhCore")
```

## Development

Run the tests with `devtools::test()`. Recent changes are listed in
[NEWS.md](NEWS.md); to contribute, see [our contributing
document](contributing.md).
