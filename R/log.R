#' Read and write evaluation logs
#'
#' @examples
#' file <-
#'   system.file(
#'     "logs/2025-02-08T15-51-00-06-00_simple-arithmetic_o3cKtmsvqQtmXGZhvfDrKB.json",
#'      package = "rinspect"
#'   )
#'
#' example_eval_log <- read_eval_log(file)
#'
#' example_eval_log
read_eval_log <- function(file) {
  structure(jsonlite::read_json(file), class = c("eval_log", "list"))
}

write_eval_log <- function(x = eval_log_new(), dir = "inst/logs/") {
  jsonlite::write_json(
    x = structure(x, class = "list"),
    path = file.path(dir, eval_log_filename(x))
  )
}
