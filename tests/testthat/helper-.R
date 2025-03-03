# an example Claude chat with:
#> <Chat turns=2 tokens=14/9>
#> ── user ─────────────────────────────────────────────────────────────────
#> What's 2+2?
#> ── assistant ────────────────────────────────────────────────────────────
#> 2+2=4
example_ellmer_solver <- function() {
  load(
    system.file(
      "sandbox/solver.rda",
      package = "rinspect"
    )
  )

  solver
}

# a log actually written by Python Inspect
example_inspect_log <- function() {
  eval_log_read(
    system.file(
      "logs/2025-02-08T15-51-00-06-00_simple-arithmetic_o3cKtmsvqQtmXGZhvfDrKB.json",
      package = "rinspect"
    )
  )
}

example_task <- function(solved = TRUE, scored = TRUE) {
  load(
    system.file(
      "sandbox/example-task.rda",
      package = "rinspect"
    )
  )

  # if it's not solved, it can't be scored!
  if (!solved) {
    tsk_data <- tsk$data()
    tsk_data <- tsk_data[
      !colnames(tsk_data) %in% c("result", "solver", "score", "scorer", "metadata")
    ]
    tsk$.__enclos_env__$private$tbl <- tsk_data
    return(tsk)
  }

  if (!scored) {
    tsk_data <- tsk$data()
    tsk_data <- tsk_data[
      !colnames(tsk_data) %in% c("result", "solver")
    ]
    tsk$.__enclos_env__$private$tbl <- tsk_data
    return(tsk)
  }

  tsk
}
