## 2026-11-07 19:17 PST — Removed publish_course(); merged update_folder() into copy_folders()

- Deleted `R/publish_course.R` (`publish_course()` and internal `setup_gh_pages_branch()`). Recoverable from git history.
- Removed `export(publish_course)` from `NAMESPACE`. `man/publish_course.Rd` still exists; to be removed when docs are regenerated with `devtools::document()`.
- Removed `update_folder()` from `R/update_lessons.R`. `update_lessons()` now calls `copy_folders()` (in `R/setup_lessons.R`), which gained `lib.loc = NULL` and `overwrite = FALSE` arguments.
- `update_lessons()` gained an `org` argument (default `"nceas-learning-hub"`).
- `update_lessons()` now also updates the `resources` folder.

## 2026-11-07 20:10 PST — Added testthat test suite

- Added `tests/testthat.R` and `tests/testthat/` with `test-init_course.R` (Git-repo location check, template validation), `test-resolve_lessons.R`, `test-define_lesson_txt.R` and `test-git_diff.R` (26 expectations, all passing).
- `DESCRIPTION`: added `Suggests: testthat (>= 3.2.0)` and `Config/testthat/edition: 3`.

## 2026-11-07 20:25 PST — Split select_latest_tag() out of query_lessons_tags()

- New internal `select_latest_tag()` in `R/query_lessons_tags.R` (returns the highest version-like tag, or `NA`); `query_lessons_tags(latest = TRUE)` calls it and warns on `NA`.
- Added `tests/testthat/test-select_latest_tag.R`.

## 2026-11-07 20:50 PST — Converted system() calls to system2()

- `checkin_lessons.R`, `compare_diffs.R` (`git_diff()`), `init_course.R` (`install_theme()`, `publish_gha()`) and `query_lessons_tags.R` now call `system2()` with the command and an argument vector instead of one pasted shell string. Behavior is unchanged; `ignore.stdout`/`ignore.stderr` became `stdout`/`stderr = FALSE`, and `intern = TRUE` became `stdout = TRUE`.

## 2026-11-07 21:15 PST — checkin_lessons(): git steps now use gert

- `checkin_lessons()` and `checkout_tmp()` (`R/checkin_lessons.R`) use `gert` (`git_clone`, `git_branch_list/create/checkout`, `git_add`, `git_status`, `git_commit`, `git_pull`, `git_push`) instead of `system2("git", ...)`; every call passes `repo =` so the working directory is never changed.
- Removed the Windows `HOME`/`USERPROFILE` workaround, which only existed for system git. Not yet verified on a machine where `HOME` differs from `USERPROFILE`.
- `checkout_tmp()` gained a `url` argument (default: the GitHub URL) so it can be pointed at a local bare repo for testing.
- `checkin_lessons()` now stops with a clear message when there is nothing to commit.
- `git_diff()` and `query_lessons_tags()` still use `system2()` (gert has no `--no-index` diff or URL-based `ls-remote`).

## 2026-11-07 21:40 PST — compare_diffs(): detect the lessons repo's default branch

- `compare_diffs(branch = )` now defaults to `NULL` (was `'main'`). New internal `get_default_branch()` and `parse_default_branch()` in `R/query_lessons_tags.R` read the remote HEAD symref via `git ls-remote --symref`. A `branch` other than the default gives a warning and is ignored (comparison is still against the installed package).
- Added `tests/testthat/test-parse_default_branch.R`.

## 2026-11-07 22:10 PST — compare_diffs() compares against a fresh GitHub install

- `compare_diffs()` now installs the lessons package from GitHub into a temporary library (`remotes::install_github()`, removed on exit) and compares against those files, so it no longer depends on the locally installed version. `branch` (default: the repo's default branch) and `pkg_version` (tag/commit/version) are now both meaningful; `pkg_version` wins if both are given.
- Removed the "installed version is older than latest tag" check and its `readline()` prompt; no longer needed.
- `search_lessons()` gained `lib.loc` so it can read a non-default install.
- `git_diff()`: stderr is no longer captured (on Windows a git line-ending warning was being read as a difference, flagging identical lessons as changed); a missing input file is now an error instead of a false "changed".
- Tests added to `tests/testthat/test-git_diff.R` for both `git_diff()` fixes.

## 2026-11-08 19:45 PST — setup_lessons(overwrite = TRUE) implemented; interactive-only check; update_lessons()/compare_diffs() fixes; NEWS and CITATION

- `R/setup_lessons.R`: removed the "not implemented" stop. New internal `remove_lessons()` deletes root-level `sNN_*.qmd` and the matching lesson folders in `images`, `data`, `slides`, `resources` (names from the qmd filenames plus `metadata_lessons.csv`) after listing them and asking y/n. `metadata_lessons.csv` is now de-duplicated (`!duplicated()`) and sorted by `basename(f_out)` in base R.
- `R/helper_functions.R`: `setup_quarto_yml(overwrite = TRUE)` resets the text between `# START SESSIONS` and `# END SESSIONS` to `# SESSION_LINKS` before writing the new list. New `check_interactive()` and `is_interactive()` (wrapper so tests can mock it).
- `check_interactive()` is called first in `init_course()`, `setup_lessons()`, `update_lessons()` and `checkin_lessons()`. Option `lhCore.allow_noninteractive` bypasses it (set only in `tests/testthat/setup.R`).
- `R/update_lessons.R`: removed the requested-vs-installed version check and the unused `installed_version`; added `upgrade = "never"` to `install_github()`; errors on requested lessons not in both course and source; records `package`/`pkg_version` in `metadata_lessons.csv`; warns when selected lessons are already at the requested version; removed the no-op `_quarto.yml` version-flag block; uses the package name actually installed in the temp library (`pkg_src`).
- `R/compare_diffs.R`: same `pkg_src` handling.
- Destructive-action warnings added to the `setup_lessons()` and `update_lessons()` docs and to their pre-prompt console messages.
- Tests: added `test-remove_lessons.R`, `test-check_interactive.R`, `setup.R`.
- Added `NEWS.md` (0.1.0) and `inst/CITATION`. `DESCRIPTION` version is 0.1.0.
