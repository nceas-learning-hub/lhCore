# lhCore 0.1.0

## Breaking changes

* `publish_course()` has been removed. Publishing is now handled by
  `init_course()` (when `setup_github = TRUE`), which creates and pushes the
  `gh-pages` branch, copies the GitHub Action and `.gitignore`, and runs
  `quarto publish gh-pages`, checking exit status along the way.
* `init_course()`: `loc = NULL` now always means the current working directory
  (it no longer depends on an `.Rproj` file being present). The location is
  checked up front, before anything is created, and the function stops with a
  message if it is inside an existing Git repository.
* `init_course()`, `setup_lessons()`, `update_lessons()` and `checkin_lessons()`
  now stop with an error when run non-interactively (e.g., from a script),
  because they modify files and prompt the user for confirmation. The
  read-only functions (`search_lessons()`, `get_lessons_version()`,
  `query_lessons_tags()`, `compare_diffs()`) are unaffected.
* `compare_diffs(branch = )` now defaults to `NULL` (was `'main'`), meaning
  the default branch of the lessons repository.

## New features

* `init_course(course_info = )` accepts all of the course information as a one-row data
  frame (e.g., read from a .csv) or a named list, in place of the individual
  arguments. Anything missing or `NA` in `course_info` gets its default; unrecognized names
  and mixing `course_info` with individual arguments are errors.

* New example dataset `test_course`, a one-row data frame of course information
  in the format accepted by `init_course(course_info = )`. Load it with
  `data(test_course)`.

* `setup_lessons(overwrite = TRUE)` is now implemented. After confirmation, it
  deletes the existing lesson qmd files (`sNN_*.qmd`) and the matching lesson
  folders in `images`, `data`, `slides` and `resources`, replaces the lesson list
  in `_quarto.yml` (between the `# START SESSIONS` and `# END SESSIONS`
  markers), and installs the new lessons. Nothing is deleted if the markers are
  missing.
* `setup_lessons()` now removes duplicated rows and sorts by output filename
  when writing `metadata_lessons.csv`. A lesson requested more than once keeps
  each instance, with its own `sNN_` prefix.
* `update_lessons()` now records the new package version in
  `metadata_lessons.csv` for the lessons it updates, warns when selected lessons
  are already recorded at the requested version, and has an `org` argument
  (default `"nceas-learning-hub"`). It also updates the `resources` folder.
* `compare_diffs()` now installs the lessons package from GitHub into a
  temporary library (removed on exit) and compares against that, so results no
  longer depend on the locally installed version. `branch` and `pkg_version`
  (tag, commit or version) are both honored; `pkg_version` wins if both are given.
* `search_lessons()` gained a `lib.loc` argument for reading a non-default
  install of the lessons package.
* `checkin_lessons()` now uses the gert package for its Git steps, and stops with
  a clear message when there is nothing to commit.
* `query_lessons_tags(latest = TRUE)` picks the highest version-like tag
  (date tags or `vX.Y.Z`), rather than requiring date-style tags.
* `setup_lessons(overwrite = TRUE)` and `update_lessons()` now print a strong
  warning before the confirmation prompt, and document that they are destructive
  and that `compare_diffs()` should be run first.

## Bug fixes

* `update_lessons()` no longer errors when the requested `version` matches the
  installed lessons package; the installed copy is irrelevant to an update.
* `update_lessons()` and `compare_diffs()` now work for older tags where the
  lessons package had a different name (e.g., `v0.0.2` installs as
  `coreRlessons`), by using the package name that was actually installed.
* `update_lessons()` now errors on requested lessons that are not in both the
  course and the source package version, and passes `upgrade = "never"` to
  `install_github()` to avoid dependency prompts.
* `compare_diffs()` / `git_diff()`: git line-ending warnings on Windows are no
  longer mistaken for differences (identical lessons were flagged as changed),
  and a missing input file is now an error instead of a false "changed".
* `search_lessons()` errors clearly when the lessons package or its `lessons`
  folder is missing, and only treats `.qmd`, `.Rmd` and `.md` files as lessons.
* Fixed regex escaping in `init_course()`.
* The startup message no longer advertises the internal
  `setup_course_structure()`.

## Internal

* `system()` calls were converted to `system2()` with argument vectors.
* `update_folder()` was merged into `copy_folders()`.
* Added a testthat test suite (edition 3) covering `init_course()` checks,
  lesson resolution, lesson text and `_quarto.yml` generation, `git_diff()`,
  tag selection, default-branch parsing, `remove_lessons()` and the interactive
  check.
