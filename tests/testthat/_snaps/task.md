# Task R6 class works

    Code
      tsk
    Output
      An evaluation task.
      Dataset: simple_addition
      Solver: generate(chat_claude())
      Scorer: model_graded_qa()

---

    Code
      tsk
    Output
      An evaluation task.
      Dataset: simple_addition
      Solver: generate(chat_claude())
      Scorer: model_graded_qa()

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

