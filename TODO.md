* test functions for:
    * check_diffs(): 
        * identify lessons that have been updated, using git diff perhaps compared to the version in lessons
        * identify new lessons (or modifications with new file name)
        * DO NOT RELY ON metadata_lessons.csv!
    * checkin_lessons() : checking in lessons from course repo back to lessons repo
        * create new branch in lessons and commit changes to there
        * generate a difference file/document to support code review
        * recognize nested files in resources or images etc
        * DON'T RELY ON metadata_course.csv for branch name - use repo name?

