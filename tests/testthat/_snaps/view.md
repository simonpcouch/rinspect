# vitals_view() errors informatively on failure to restart

    Code
      vitals_view(log_dir)
    Condition
      Error in `vitals_view()`:
      ! Unable to terminate the existing server.
      Caused by error in `httpuv::stopServer()`:
      ! no way!

# vitals_view() handles nonexistent log directory

    Code
      vitals_view(log_dir)
    Condition
      Error in `vitals_view()`:
      ! Log directory '/path/that/does/not/exist' not found.

