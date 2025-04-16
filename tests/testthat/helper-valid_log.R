# validating log files (for dev use only) --------------------------------------
# invokes Inspect's pydantic models on an eval log file so that
# we can ensure we're writing files that are compatible with the
# viewer.
expect_valid_log <- local({
  .pydantic_skip_status <- if (!interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
    "On CRAN."
  } else if (system2("python", "--version", stdout = FALSE, stderr = FALSE) != 0) {
    "Python is not available"
  } else if (system2("python", "-c 'import inspect_ai'", stdout = FALSE, stderr = FALSE) != 0) {
    "inspect_ai Python module is not available"
  } else if (system2("python", "-c 'import pydantic'", stdout = FALSE, stderr = FALSE) != 0) {
    "pydantic Python module is not available"
  } else if (!file.exists(system.file("test/validate_log.py", package = "vitals"))) {
    "Python validation script not found."
  } else {
    NULL
  }
  
  function(x) {
    if (!is.null(.pydantic_skip_status)) {
      skip(.pydantic_skip_status)
    }
    
    if (!file.exists(x)) {
      skip(paste0("Log file ", x, " does not exist."))
    }
    
    result <- system2(
      "python",
      args = c(system.file("test/validate_log.py", package = "vitals"), x),
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
})
