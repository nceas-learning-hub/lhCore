## 2026-11-10 15:00 PST — Update lhCore for lhLessons folder restructuring

Reorganized lesson content structure in `lhLessons` from four parallel folders (`lesson_images/`, `lesson_data/`, `lesson_slides/`, `lesson_resources/`) to a nested structure (`lessons/images/`, `lessons/data/`, `lessons/slides/`, `lessons/resources/`), mirroring the final course website structure and eliminating cognitive overhead when working with lesson content.

**Changes to lhCore:**
- Updated `setup_lessons()`: `from` paths now point to `lessons/images`, `lessons/data`, `lessons/slides`, `lessons/resources` instead of `lesson_*` folders.
- Updated `update_lessons()`: same folder path changes in the loop that calls `copy_folders()`.
- Updated `checkin_lessons()` helper `copy_files_to_checkin()`: destination path now uses `inst/lessons/{folder}` instead of `inst/lesson_{folder}`.
- Updated documentation in all three functions to refer to "lesson subfolders" rather than "lesson folders" for clarity.
- Verified `search_lessons()` and `query_lessons_tags()` are unaffected (they only reference `lessons/` for `.qmd` files).

**Rationale:** The new structure is self-documenting: navigating to one lesson's folder in `lhLessons` now reveals everything needed for that lesson (its images, data, slides, resources) in organized subfolders, just as they appear in the final course site. The nested approach also enables potential future sharing of assets across lessons without creating parallel-folder confusion.

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

## 2026-11-08 19:45 PST — Overwrite wipes lesson files and folders; functions that change files are interactive-only

`setup_lessons(overwrite = TRUE)` deletes the existing lesson qmds and matching lesson folders, then rewrites the `_quarto.yml` lesson list between `# START SESSIONS` / `# END SESSIONS`. It checks the markers first and asks for confirmation, so nothing is deleted if the yml can't be reset. Lesson files are identified by the `sNN_` pattern, so hand-made files using that pattern would also be removed (they are listed before the prompt).

- Only functions that change files (`init_course()`, `setup_lessons()`, `update_lessons()`, `checkin_lessons()`) must run interactively. Read-only functions (`search_lessons()`, `get_lessons_version()`, `query_lessons_tags()`, `compare_diffs()`) stay script-friendly. `compare_diffs()` does write `git_diff.txt` but was deliberately left unrestricted.
- Tests bypass the check with an option rather than mocking `interactive()` (a primitive; the mock had no effect in a non-interactive run), hence the `is_interactive()` wrapper.
- `update_lessons()` no longer compares the requested version to the installed lessons package: an update always installs the requested version into a temp library, so the installed copy is irrelevant (and the check failed when a course built from an old version was updated with the latest installed). Replaced by a warning when selected lessons are already recorded at that version in `metadata_lessons.csv`.
- The version is recorded in `metadata_lessons.csv`; the `_quarto.yml` `###` version comments were dropped because `setup_lessons()` never wrote them.
- Older tags can install under a different package name (`v0.0.2` is `coreRlessons`), so `update_lessons()` and `compare_diffs()` use the name actually found in the temp library.
- `DESCRIPTION` had `person("O'Hara", "Casey", ...)` (given/family reversed); fixed to `person("Casey", "O'Hara", ...)` to match `inst/CITATION`.

## 2026-11-10 15:15 PST — Breaking change: folder structure reorganization and version checks

Reorganized lesson content structure in `lhLessons` from four parallel folders (`lesson_images/`, `lesson_data/`, `lesson_slides/`, `lesson_resources/`) to nested structure (`lessons/images/`, `lessons/data/`, `lessons/slides/`, `lessons/resources/`). This mirrored the course website structure exactly and reduced cognitive overhead.

**Version increments:**
- lhCore: 0.1.0 → 0.2.0 (breaking change)
- lhLessons: 2026.11.08 → 2026.11.10 (breaking change)

**Compatibility guarantee:**
Added `check_lhlessons_compatibility()` to prevent incompatible version pairs:
- lhCore 0.2.0+ requires lhLessons 2026.11.10+
- lhCore 0.1.x requires lhLessons 2026.11.08 or earlier

The check runs at the start of `setup_lessons()`, `update_lessons()`, and `search_lessons()` to catch mismatches early with a clear error message.

**Rationale:** The folder restructuring is a breaking change because the paths passed to `copy_folders()` changed fundamentally. A version guard prevents silent failures where old lhCore tries to copy from non-existent `lesson_*` folders, or new lhCore fails because old lhLessons lacks the nested structure.
