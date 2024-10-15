### A function to update lessons based on a version of the coreR R package
update_lessons <- function(lessons = "all", version = NULL) {
  ### placeholder - give a vector of lesson names, optionally give a version tag
  ### if version tag is NULL use latest
  ### maybe version tag is a commit hash, a date, a version number, or a release?
  ### could also give a flag of lessons = 'all' maybe, so it updates all update-able lessons
  ### is there a way to track which lessons are custom (and not update those)?
  ### * maybe locally, make a copy with a different name?
  ### * if filename doesn't match an "official" lesson then it won't get updated (with a warning)
}
