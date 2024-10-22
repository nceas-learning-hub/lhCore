.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to coreR!  Key functionality (see documentation for details):",
    "\n\u2022 `init_course()` to set up a new course repository",
    "\n\u2022 `setup_lessons()` to assemble a chosen set of lessons into a course",
    "\n\u2022 `update_lessons()` to update/revert course to a different version of the lessons package",
    "\n\u2022 `search_lessons()` to search for lessons in the lessons package",
    "\n\u2022 `available_lessons()` to list the lessons available in the lessons package",
    "\n\u2022 `get_lessons_version()` to see the version of the installed lessons package"
  )
}
