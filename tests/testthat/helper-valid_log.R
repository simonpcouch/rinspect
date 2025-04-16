# validating log files (for dev use only) --------------------------------------
# invokes Inspect's pydantic models on an eval log file so that
# we can ensure we're writing files that are compatible with the
# viewer.
expect_valid_log <- function(x) {
  # TODO: each of these skips can probably just be evaluated once in a `local()`
  skip_on_cran()

  skip_if_not(
    system2("python", "--version", stdout = FALSE, stderr = FALSE) == 0,
    message = "Python is not available"
  )
    
  skip_if_not(
    system2("python", "-c 'import inspect_ai'", stdout = FALSE, stderr = FALSE) == 0,
    message = "inspect_ai Python module is not available"
  )
    
  skip_if_not(
    system2("python", "-c 'import pydantic'", stdout = FALSE, stderr = FALSE) == 0,
    message = "pydantic Python module is not available"
  )

  if (!file.exists(x)) {
    skip("Log file {x} does not exist.")
  }
  
  py_script <- system.file("test/validate_log.py", package = "vitals")
  
  if (!file.exists(py_script)) {
    skip("Python validation script {py_script} not found.")
  }
  
  result <- system2(
    "python",
    args = c(py_script, x),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  
  expect(
    is.null(status) || status == 0,
    paste0(
      c("The generated log did not pass the pydantic model: ", result),
      collapse = ""
    )
  )
}
