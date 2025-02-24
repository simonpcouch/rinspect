#' Read and write evaluation logs
#'
#' @description
#' Utilities for reading and writing evaluation logs.
#'
#' These utilities support the `INSPECT_LOG_DIR` environment variable,
#' which sets a default directory to write logs to.
#'
#' @param x For `eval_log_read()`, a path to the log file to read. For
#' For `eval_log_write()`, an evaluation log object to write to disk.
#' @param dir Directory where logs should be stored.
#'
#' @examples
#' file <-
#'   system.file(
#'     "logs/2025-02-08T15-51-00-06-00_simple-arithmetic_o3cKtmsvqQtmXGZhvfDrKB.json",
#'      package = "rinspect"
#'   )
#'
#' example_eval_log <- eval_log_read(file)
#'
#' example_eval_log
#' @name eval_log
#' @export
eval_log_read <- function(x) {
  structure(jsonlite::read_json(x), class = c("eval_log", "list"))
}

#' @rdname eval_log
#' @export
eval_log_write <- function(x = eval_log_new(), dir = eval_log_dir()) {
  if (!dir.exists(dir)) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }

  jsonlite::write_json(
    x = structure(x, class = "list"),
    path = file.path(dir, eval_log_filename(x))
  )
}

#' @rdname eval_log
#' @export
eval_log_dir <- function() {
  Sys.getenv("INSPECT_LOG_DIR", unset = "./logs")
}

#' @rdname eval_log
#' @export
eval_log_dir_set <- function(dir) {
  old <- Sys.getenv("INSPECT_LOG_DIR", unset = NA)
  Sys.setenv(INSPECT_LOG_DIR = path)
  invisible(old)
}
