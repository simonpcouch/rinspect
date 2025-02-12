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
  read_eval_log(
    system.file(
      "logs/2025-02-08T15-51-00-06-00_simple-arithmetic_o3cKtmsvqQtmXGZhvfDrKB.json",
      package = "rinspect"
    )
  )
}
