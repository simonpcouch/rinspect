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
  # loads a cached `tsk` with example output.
  # regenerate with:
  #> tsk <- Task$new(
  #>   dataset = simple_addition, 
  #>   solver = generate(chat_claude()), 
  #>   scorer = model_graded_qa()
  #> )
  #> 
  #> tsk$eval()
  #>
  #> save(tsk, file = "inst/test-objects/example-task.rda")
  load(
    system.file(
      "test-objects/example-task.rda",
      package = "rinspect"
    )
  )

  simple_addition <- tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )
  
  res <- Task$new(
    dataset = simple_addition, 
    solver = function(...) {}, 
    scorer = function(...) {}
  )

  if (!solved) {
    return(res)
  }

  res$samples$result <- tsk$samples$result
  res$samples$solver_chat <- tsk$samples$solver_chat
  res$.__enclos_env__$private$solved <- TRUE

  if (!scored) {
    return(res)
  }

  res$samples$score <- tsk$samples$score
  res$samples$scorer_chat <- tsk$samples$scorer_chat
  res$.__enclos_env__$private$scored <- TRUE

  res
}
