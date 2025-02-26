#' The R Eval
#'
#' @description
#' The R Eval is a dataset of challenging R coding problems. Each `input` is a
#' question about R code which could be solved on first-read only by human
#' experts and, with a chance to read documentation and run some code, by 
#' fluent data scientists. Solutions are in `target()` and enable a fluent 
#' data scientist to evaluate whether the solution deserves full, partial, or
#' no credit.
#' 
#' Pass this dataset to [task_create()] to situate it inside of an evaluation
#' task.
#' 
#' @format A tibble with `r nrow(tre)` rows and `r ncol(tre)` columns:
#' \describe{
#'   \item{title}{Character. Unique identifier/title for the code problem.}
#'   \item{input}{Character. The question to be answered.}
#'   \item{target}{Character. The solution, often with a description of notable
#'   features of a correct solution.}
#'   \item{domain}{Character. The technical domain 
#'   (e.g., Data Analysis, Programming, or Authoring).}
#'   \item{task}{Character. Type of task 
#'   (e.g., Debugging, New feature, or Translation.)}
#'   \item{source}{Character. URL or source of the problem. `NA`s indicate that
#'   the problem was written originally for this eval.}
#'   \item{knowledge}{List. Required knowledge/concepts for solving the problem.}
#' }
#'
#' @source Posit Community, GitHub issues, R4DS solutions, etc. For row-level
#' references, see `source`.
#' 
#' @examples
#' tre
#' 
#' dplyr::glimpse(tre)
"tre"
