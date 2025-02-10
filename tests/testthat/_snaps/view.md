# inspect_view() works with .json from Python Inspect

    Code
      inspect_view(log_dir = log_dir)
    Message
      v Inspect Viewer running at: <http://127.0.0.1:7576>

# inspect_view() restarts with existing server

    Code
      inspect_view(log_dir = log_dir)
    Message
      v Inspect Viewer running at: <http://127.0.0.1:7576>

# inspect_view() errors informatively on failure to restart

    Code
      inspect_view()
    Condition
      Error in `inspect_view()`:
      ! Unable to terminate the existing server.
      Caused by error in `httpuv::stopServer()`:
      ! no way!

# inspect_view() handles nonexistent log directory

    Code
      inspect_view(log_dir = log_dir)
    Condition
      Error in `inspect_view()`:
      ! Log directory '/path/that/does/not/exist' not found.

