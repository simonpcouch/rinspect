# Task R6 class works

    Code
      tsk
    Output
      An evaluation task simpleaddition.

---

    Code
      tsk
    Output
      An evaluation task simpleaddition.
      Explore interactively with `.last_task$view()`.

# check_dataset works

    Code
      Task$new(dataset = data.frame(input = 1), solver = function() { }, scorer = function()
        { })
    Condition
      Error in `initialize()`:
      ! `dataset` is missing required column target.

---

    Code
      Task$new(dataset = data.frame(target = 1), solver = function() { }, scorer = function()
        { })
    Condition
      Error in `initialize()`:
      ! `dataset` is missing required column input.

---

    Code
      Task$new(dataset = data.frame(x = 1), solver = function() { }, scorer = function()
        { })
    Condition
      Error in `initialize()`:
      ! `dataset` is missing required columns input and target.

# Task errors informatively with duplicate ids

    Code
      Task$new(dataset = d, solver = function() { }, scorer = function() { })
    Condition
      Error in `initialize()`:
      ! Duplicated values found in the id column. Each ID must be unique.

# set_solver works

    Code
      .res <- tsk$set_solver(new_solver)
    Condition
      Warning:
      Clearing results from previous solver.

---

    Code
      .res <- tsk$set_solver(new_solver)
    Condition
      Warning:
      Clearing results from previous solver.

# set_scorer works

    Code
      .res <- tsk$set_scorer(scorer_chat)
    Condition
      Warning:
      Clearing scores from previous scorer.

---

    Code
      .res <- tsk$set_scorer(scorer_metadata)
    Condition
      Warning:
      Clearing scores from previous scorer.

# Task completeness is tracked and preserved

    Code
      .res <- tsk$set_solver(generate(chat_anthropic(model = "claude-3-7-sonnet-latest")))
    Condition
      Warning:
      Clearing results from previous solver.

---

    Code
      .res <- tsk$set_scorer(mock_scorer)
    Condition
      Warning:
      Clearing scores from previous scorer.

# Task errors informatively with bad solver output

    Code
      tsk$solve()
    Condition
      Error in `$solve`:
      ! `solver` must return slots result and solver_chat.

# Task detects non-Chat objects in solver_chat

    Code
      tsk$solve()
    Condition
      Error in `$solve`:
      ! Elements in the solver_chat output from `solver` must be ellmer Chat objects, not a string.

