# check_inherits works

    Code
      check_inherits(1, "list")
    Condition
      Error:
      ! `1` must be a <list>, not a number

---

    Code
      check_inherits(1, "list", x_arg = "my_arg")
    Condition
      Error:
      ! `my_arg` must be a <list>, not a number

