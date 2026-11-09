### testthat runs non-interactively; bypass the interactive-only check in
### check_interactive() for the duration of the test run
withr::local_options(lhCore.allow_noninteractive = TRUE, .local_envir = teardown_env())
