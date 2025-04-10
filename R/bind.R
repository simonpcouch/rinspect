#' Concatenate task samples for analysis
#'
#' @description
#' Combine multiple [Task] objects into a single tibble for comparison.
#' 
#' This function takes multiple (optionally named) [Task] objects and row-binds 
#' their `$samples` together, adding a `task` column to identify the source of each 
#' row. The resulting tibble nests additional columns into a `metadata` column
#' and is ready for further analysis.
#'
#' @param ... `Task` objects to combine, optionally named.
#'
#' @returns A tibble with the combined samples from all tasks, with a `task`
#' column indicating the source and a nested `metadata` column containing
#' additional fields.
#' 
#' @export
vitals_bind <- function(...) {
  x <- dots_list(..., .named = TRUE)
  lapply(x, check_inherits, cls = "Task", call = caller_env(0))
  
  x <- purrr::map2(x, names(x), function(task, task_name) {dplyr::mutate(
    task$samples,
    task = task_name,
    .before = everything()
  )})

  res <- purrr::list_rbind(x)
  res <- tidyr::nest(
    res,
    .by = any_of(c("task", "id", "epoch", "score")),
    .key = "metadata"
  )
  res
}
