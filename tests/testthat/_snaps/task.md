# basic task_create -> task_solve -> task_score works

    Code
      tsk
    Output
      # Evaluation task simple_addition.
      # A tibble: 2 x 3
        input       target    id
      * <chr>       <chr>  <int>
      1 What's 2+2? 4          1
      2 What's 2+3? 5          2

# check_dataset works

    Code
      task_create(data.frame(input = 1))
    Condition
      Error in `task_create()`:
      ! `dataset` is missing required column target.

---

    Code
      task_create(data.frame(target = 1))
    Condition
      Error in `task_create()`:
      ! `dataset` is missing required column input.

---

    Code
      task_create(data.frame(x = 1))
    Condition
      Error in `task_create()`:
      ! `dataset` is missing required columns input and target.

