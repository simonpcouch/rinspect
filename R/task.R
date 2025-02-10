#' Evaluation tasks
#'
#' @description
#' Evaluation `tasks` provide a flexible data structure for evaluating LLM-based
#' tools.
#' 
#' @param dataset A tibble with, minimally, columns `input` and `target`.
#' @param solver A function that takes an element of `dataset$input` as input
#' and returns a value approximating `dataset$target` or an [ellmer::Chat]
#' object.
#' @param scorer A function that evaluates how well the solver's return value
#' approximates the corresponding elements of `dataset$target`.
#' @param ... Named task parameters. The resulting `task` will accept any of 
#' these parameters allowing for easily running variants of the task without
#' changing its source code.
#'
#' @examples
#' \dontrun{
#'
#' library(ellmer)
#' library(tibble)
#' library(glue)
#'
#' dataset <- tibble(
#'   input = c("What's 2+2?", "What's 2+3?"),
#'   target = c("4", "5")
#' )
#'
#' # requires an ANTHROPIC_API_KEY
#' solver <- function(input, ch = chat_claude()) {
#'   ch$clone()$chat(input)
#' }
#' 
#' # or, for more metadata...
# TODO: we probably actually need to figure to detect when a `$chat()` method is
# triggered and hook into it so that users can supply arbitrarily complex solvers
#' solver <- chat_claude()
#'
#' scorer <- function(input, target, output) {
#'   res <- chat_claude()$chat(glue::glue(
#'     "An assistant was asked the following: {input}\n",
#'     "The answer is: {target}.\n",
#'     "The assistant responded: {output}.\n",
#'     "Was the assistant correct?\n",
#'     "End your response with 'Answer: Yes' if yes, 'Answer: No' if no."
#'   ))
#'
#'   if (grepl("Answer: Yes", res)) {
#'     return(TRUE)
#'   } else if (grepl("Answer: No", res)) {
#'     return(FALSE)
#'   } else {
#'     return(NA)
#'   }
#' }
#' 
#' task_new(
#'   dataset = dataset,
#'   solver = solver,
#'   scorer = scorer
#' )
#' }
#' @aliases task
#' @export
# TODO: add task options (https://inspect.ai-safety-institute.org.uk/tasks.html#task-options)
task_new <- function(dataset, solver, scorer, ..., name = generate_id()) {
  check_data_frame(dataset)
  if (!is_missing(solver) && inherits(solver, "Chat")) {
    solver_chat <- solver
    solver <- function(input) {solver_chat$clone()$chat(input)}
  } else {
    solver_chat <- NA
    check_function(solver)
  }
  check_function(scorer)

  res <- carrier::crate(
    function(...) {
      dataset$output <- character(nrow(dataset))
      dataset$scores <- logical(nrow(dataset))
      for (i in seq_len(nrow(dataset))) {
        sample <- dataset[i, , drop = FALSE]
        dataset$output[i] <- solver(sample$input, ...)
        dataset$scores[i] <- scorer(
          input = sample$input,
          target = sample$target,
          output = dataset$output[i],
          ...
        )
      }
    
      dataset
    },
    dataset = dataset,
    solver = solver,
    solver_chat = solver_chat,
    scorer = scorer,
    name = name
  )

  structure(res, class = c("task", class(res)))
}

#' @export
print.task <- function(x, ...) {
  # TODO: add "with parameters..."
  cli::cat_line(cli::format_inline("An evaluation {cli::col_blue('task')}."))
}

#' @rdname task_new
task_evaluate <- function(task, ...) {
  check_inherits(task, "task")

  result <- task(...)

  # eval_log(task = task, ..., result = result)

  result
}