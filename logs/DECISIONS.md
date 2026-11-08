## 2026-11-07 19:17 PST — Retire publish_course(); publishing is handled by init_course()

`init_course()` now sets up GitHub and runs `publish_gha()`, which creates and pushes `gh-pages`, copies the GitHub Action and `.gitignore`, and runs `quarto publish gh-pages --no-prompt`, with exit-status checks. `publish_course()` duplicated that with unchecked `system()` git calls, appended a blank line to the user's `index.qmd` to force a commit, and could leave the user on the wrong branch after an error.

- Trade-off accepted: there is no exported way to publish a course whose repo was not created by `init_course()` (or was created with `setup_github = FALSE`). If needed later, export a renamed `publish_gha()` instead of restoring the old function.
- Rejected alternative: keep `publish_course()` and rework it to call `publish_gha()`.

## 2026-11-07 19:17 PST — Package code review: bugs fixed in checkin_lessons, compare_diffs, setup_lessons, update_lessons, init_course

Reviewed `R/` file by file and fixed confirmed bugs (see `git diff`). Items deliberately left alone as design questions: interactive `readline()` prompts, hard-coded theme org in `install_theme()`, the unused `pkg_version` and `branch` arguments in `compare_diffs()`, and `update_lessons()` editing `_quarto.yml` version comments that `setup_lessons()` never writes. Remaining files not yet reviewed: `query_lessons_tags.R`, `helper_functions.R`, `search_lessons.R`.

## 2026-11-07 19:45 PST — Code review complete: search_lessons.R, onAttach.R, query_lessons_tags.R, helper_functions.R

Finished the file-by-file pass. `search_lessons()` now errors clearly when the lessons package or its `lessons` folder is missing, and only treats .qmd/.Rmd/.md files as lessons. `query_lessons_tags(latest = TRUE)` now picks the highest version-like tag (date tags or `vX.Y.Z`) instead of requiring `20xx.xx.xx` date tags. The startup message no longer advertises the internal `setup_course_structure()`; it points to `search_lessons()` instead.

## 2026-11-07 19:58 PST — init_course(): replace .Rproj check with an up-front "not inside a Git repo" check

The old default (`loc = '..'` if an `.Rproj` was in the working directory) doesn't fit Positron projects, which may have no `.Rproj`. The real hazard is nesting the new course repo inside an existing Git repo, so `init_course()` now calls `gert::git_find()` on `loc` (or its nearest existing parent) before creating anything and stops with a message if a repo is found. `loc = NULL` now always means the working directory.

- Rejected alternative: when `loc = NULL` and inside a repo, fall back to the repo's parent folder (could surprise the user about where the course lands).
- Trade-off accepted: a location that is legitimately inside a tracked folder is rejected; there is no override argument.

## 2026-11-07 22:10 PST — compare_diffs() always compares against a fresh install, not the local one

The installed copy of the lessons package can be older than GitHub, which made diffs misleading and needed a version-check prompt. Installing the requested branch/version into a temp library each run makes the comparison independent of local state and lets `branch`/`pkg_version` work.

- Trade-off accepted: each call needs network access and a package install (slower; uses `upgrade = "never"` to avoid dependency prompts).
- `file_remote` in the returned data frame points into the temp library, which is deleted on return.
