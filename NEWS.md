# vitals (development version)

* The package will now set the envvar `IN_VITALS_EVAL` to `"true"` during 
  solving and scoring.

* Numeric task targets will no longer introduce errors in the log viewer.

* `detect_match()` now lists the correct `location` options in its default 
  value (#140, #142 by @mattwarkentin).

* The log viewer previously reported the scorer's response as both the solver's
  and scorers response—this is now fixed (#141, #142 by @mattwarkentin).

# vitals 0.1.0

* Initial CRAN submission.
