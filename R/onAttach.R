.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to lhCore!  Key functionality (see documentation for details):",
    "\n\u2022 `init_course()` to set up a new course repository",
    "\n\u2022 `setup_course_structure()` to establish the skeleton of the course, including template",
    "\n\u2022 `setup_lessons()` to assemble a chosen set of lessons into a course",
    "\nCheck out https://nceas-learning-hub.github.io/lh_packages/ for details!"
  )
}
