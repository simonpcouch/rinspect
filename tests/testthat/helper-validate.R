# validating log files (for dev use only) --------------------------------------
# invokes Inspect's pydantic models on an eval log file so that
# we can ensure we're writing files that are compatible with the
# viewer.
validate_log <- function(x) {
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
    cli::cli_abort("Log file {x} does not exist.")
  }
  
  py_script <- system.file("test/validate_log.py", package = "vitals")
  
  if (!file.exists(py_script)) {
    cli::cli_abort("Python validation script {py_script} not found.")
  }
  
  result <- system2(
    "python",
    args = c(py_script, x),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  
  if (!is.null(status) && status > 0) {
    cli::cli_abort("{result}")
  }
  
  cli::cli_alert_success("Log file validated successfully")
  return(invisible(NULL))
}
