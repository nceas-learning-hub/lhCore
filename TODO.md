* Change name to something like lhCore and lhLessons
* Set up the documentation site in a new repo like lhPackages
* add execute: _freeze: auto in the _quarto.yml and add .github/workflows/publish.yml for auto publishing
    * maybe this is an option in the setup or in a `publish_course()` function?
* add templates for ADC, coreR, Delta, USGS, Working Groups, etc.
    * template-specific index, header css (icons, gradient), other options?
    * same lh_style scss files though for simplicity and branding
* develop functions for:
    * check_diffs(): 
        * identify lessons that have been updated, using git diff perhaps compared to the version in lessons
        * identify new lessons (or modifications with new file name)
    * checkin_lessons() : checking in lessons from course repo back to lessons repo
        * create new branch in lessons and commit changes to there
        * generate a difference file/document to support code review
    * publish_course(): publish to Github.io with GHA
