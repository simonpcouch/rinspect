# validating log files (for dev use only)
# invokes Inspect's pydantic models on an eval log file so that
# we can ensure we're writing files that are compatible with the
# viewer.
validate_log <- function(x) {
  if (!file.exists(x)) {
    cli::cli_abort("Log file {x} does not exist.")
  }
  
  py_script <- system.file("pydantic/validate_log.py", package = "rinspect")
  
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
