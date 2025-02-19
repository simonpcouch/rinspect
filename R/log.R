#' Read and write evaluation logs
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
eval_log_read <- function(file) {
  structure(jsonlite::read_json(file), class = c("eval_log", "list"))
}

#' @rdname eval_log
#' @export
eval_log_write <- function(x = eval_log_new(), dir = eval_log_dir()) {
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
