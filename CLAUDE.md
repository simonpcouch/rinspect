You are situated inside of an R package source directory. The subdirectory `R/` contains source files. The subdirectory `tests/testthat/` contains corresponding tests. e.g. `R/task.R` is tested primarily in `tests/testthat/test-task.R`.

Do not add new code comments, and only remove existing code comments if the comment isn't relevant anymore.

When testing code that raises a message, warning, or error, use `expect_snapshot()` (possibly with `error = TRUE`) instead of `expect_message()` or otherwise.

To get a sense for the style used to write and test code, read `R/task.R` and `tests/testthat/test-task.R`, respectively.
