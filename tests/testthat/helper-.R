# an example Claude chat with:
#> <Chat turns=2 tokens=14/9>
#> ── user ─────────────────────────────────────────────────────────────────
#> What's 2+2?
#> ── assistant ────────────────────────────────────────────────────────────
#> 2+2=4
example_ellmer_solver <- function() {
  load(
    system.file(
      "test/solver.rda",
      package = "vitals"
    )
  )

  solver
}

# a log actually written by Python Inspect
example_inspect_log <- function() {
  log_path <- system.file(
    "test/inspect/logs/2025-03-24T10-39-36-05-00_simple-arithmetic_fQ9mYnqZFhtEuUenPpJgKL.json",
    package = "vitals"
  )
  if (identical(log_path, "")) {
    testthat::skip("Test log files not available")
  }
  eval_log_read(log_path)
}

example_task <- function(solved = TRUE, scored = TRUE) {
  # loads a cached `tsk` with example output.
  # regenerate with `inst/regenerate-example-objects.R`
  load(
    system.file(
      "test/example-task.rda",
      package = "vitals"
    )
  )

  simple_addition <- tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  res <- Task$new(
    dataset = simple_addition,
    solver = function(...) {
    },
    scorer = function(...) {
    }
  )

  if (!solved) {
    return(res)
  }

  res$.__enclos_env__$private$samples$result <- tsk$get_samples()$result
  res$.__enclos_env__$private$samples$solver_chat <- tsk$get_samples()$solver_chat
  res$.__enclos_env__$private$solved <- TRUE

  if (!scored) {
    return(res)
  }

  res$.__enclos_env__$private$samples$score <- tsk$get_samples()$score
  res$.__enclos_env__$private$samples$scorer_chat <- tsk$get_samples()$scorer_chat
  res$.__enclos_env__$private$scored <- TRUE

  res
}
