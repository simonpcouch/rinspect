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
    return(tsk[
      !colnames(tsk) %in% c("result", "solver", "score", "scorer")
    ])
  }

  if (!scored) {
    return(tsk[
      !colnames(tsk) %in% c("score", "scorer")
    ])
  }

  tsk
}
