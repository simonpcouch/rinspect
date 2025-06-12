# vitals_bundle creates a valid bundle

    Code
      vitals_bundle(log_dir = log_dir, output_dir = output_dir, overwrite = FALSE)
    Condition
      Error in `vitals_bundle()`:
      ! '`output_dir`' already exists.
      i Choose another output directory or use overwrite = TRUE.

---

    Code
      result <- vitals_bundle(log_dir = log_dir, output_dir = output_dir, overwrite = TRUE)
    Message
      v Bundle '***' created!

# vitals_bundle errors informatively

    Code
      vitals_bundle(log_dir = empty_log_dir, output_dir = output_dir)
    Condition
      Error in `vitals_bundle()`:
      ! The log directory '***' doesn't contain any JSON log files.

---

    Code
      vitals_bundle(log_dir = non_existent_dir, output_dir = output_dir)
    Condition
      Error in `vitals_bundle()`:
      ! `log_dir` '***' doesn't exist.

