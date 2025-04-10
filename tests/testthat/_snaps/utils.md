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

# check_log_dir warns informatively

    Code
      res <- Task$new(tibble(input = 1, target = 1), function() { }, function() { })
    Condition
      Warning:
      ! vitals could not find a log directory; evaluation log files will be written to a temporary directory.
      i Set a log directory with e.g. `vitals::vitals_log_dir_set("./logs")`, perhaps in '~/.Rprofile', to quiet this warning.
