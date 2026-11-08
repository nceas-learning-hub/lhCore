#' Query the tags of the lessons package remote
#'
#' @param pkg The name of the lessons package repository (e.g., "lhLessons")
#' @param org The GitHub organization where the lessons package repo lives.
#'        Default "nceas-learning-hub"
#' @param latest Return only the latest tag?  Default `FALSE` returns all tags.
#' @param quiet Suppress display of the version as a message?
#'
#' @return Vector of tags for the package, as characters.
#' @export
#'
#' @examples \dontrun{query_lessons_tags(pkg = 'lhLessons', latest = TRUE)}
#'
query_lessons_tags <- function(pkg, org = 'nceas-learning-hub',
                             latest = FALSE,
                             quiet  = FALSE) {
  repo_url <- sprintf('https://github.com/%s/%s', org, pkg)
  sys_out <- suppressWarnings(system2('git', c('ls-remote', '--tags', repo_url), stdout = TRUE))
  ### system2(stdout = TRUE) attaches a "status" attribute only on failure
  if(!is.null(attr(sys_out, 'status'))) {
    stop('Unable to list tags for ', org, '/', pkg, ' (git ls-remote failed; ',
         'check the repository name, your network connection, and that git is installed)')
  }
  ### annotated tags appear twice, the second time as "<tag>^{}" - drop those
  tags_vec <- sub('^.+refs/tags/', '', sys_out)
  tags_vec <- tags_vec[!grepl('\\^\\{\\}$', tags_vec)]
  if(!quiet) message(pkg, ' tags: \n', paste0('   ', tags_vec, collapse = '\n'))
  if(latest) {
    latest_tag <- select_latest_tag(tags_vec)
    if(is.na(latest_tag)) {
      warning('No version-like tags found for ', org, '/', pkg, '; returning NA')
    }
    return(latest_tag)
  }
  return(tags_vec)
}

### not exported
get_default_branch <- function(pkg, org = 'nceas-learning-hub') {
  ### name of the default branch of the lessons repo on GitHub, from the remote
  ### HEAD symref (no clone or local repo needed); NA if the remote doesn't report one
  repo_url <- sprintf('https://github.com/%s/%s', org, pkg)
  sys_out <- suppressWarnings(system2('git', c('ls-remote', '--symref', repo_url, 'HEAD'),
                                      stdout = TRUE))
  if(!is.null(attr(sys_out, 'status'))) {
    stop('Unable to query the default branch of ', org, '/', pkg, ' (git ls-remote failed)')
  }
  parse_default_branch(sys_out)
}

### not exported
parse_default_branch <- function(ls_remote_lines) {
  ### `git ls-remote --symref <url> HEAD` prints "ref: refs/heads/<branch>\tHEAD"
  ### followed by the commit hash line
  is_ref <- grepl('^ref: refs/heads/(.+)\\tHEAD$', ls_remote_lines)
  branch <- sub('^ref: refs/heads/(.+)\\tHEAD$', '\\1', ls_remote_lines[is_ref])
  if(length(branch) == 0) return(NA_character_)
  return(branch[1])
}

### not exported
select_latest_tag <- function(tags) {
  ### latest = highest version among tags that parse as versions, with an
  ### optional leading "v" (handles both date tags like 2024.10.01 and v0.1.2).
  ### Returns the original tag string, or NA if no tag looks like a version.
  tag_ver <- suppressWarnings(numeric_version(sub('^v', '', tags), strict = FALSE))
  valid <- !is.na(tag_ver)
  if(!any(valid)) return(NA_character_)
  ### (max() on numeric_version drops to NA with na.rm, so order instead)
  return(tags[valid][order(tag_ver[valid], decreasing = TRUE)[1]])
}
