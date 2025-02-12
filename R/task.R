#' Evaluation tasks
#'
#' @description
#' Evaluation `tasks` provide a flexible data structure for evaluating LLM-based
#' tools.
#' 
#' @param dataset A tibble with, minimally, columns `input` and `target`.
#' @param solver A function that takes an element of `dataset$input` as input
#' and determines a value approximating `dataset$target`. Its return value should
#' be a list with elements `result` (the final response) and `chat` (an ellmer
#' chat used to solve the problem, or a list of them).
#' 
#' Alternatively, just supply an [ellmer::Chat] object!
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
#' solver <- function(input, chat = chat_claude()) {
#'   ch <- chat$clone()
#'   res <- ch$chat(input)
#' 
#'   list(result = res, chat = ch)
#' }
#' 
#' # or, equivalently
#' solver <- chat_claude()
#'
#' scorer <- function(input, target, output) {
#'   ch <- chat_claude()
#'   response <- ch$chat(glue::glue(
#'     "An assistant was asked the following: {input}\n",
#'     "The answer is: {target}.\n",
#'     "The assistant responded: {output}.\n",
#'     "Was the assistant correct?\n",
#'     "End your response with 'Answer: Yes' if yes, 'Answer: No' if no."
#'   ))
#'
#'   if (grepl("Answer: Yes", response)) {
#'     res <- TRUE
#'   } else if (grepl("Answer: No", response)) {
#'     res <- FALSE
#'   } else {
#'     res <- NA
#'   }
#' 
#'   list(result = res, chat = ch)
#' }
#' 
#' tsk <- task_new(
#'   name = "simple_addition",
#'   dataset = dataset,
#'   solver = solver,
#'   scorer = scorer
#' )
#' 
#' task_evaluate(tsk)
#' }
#' @aliases task
#' @export
# TODO: add task options (https://inspect.ai-safety-institute.org.uk/tasks.html#task-options)
task_new <- function(name, dataset, solver, scorer, ...) {
  check_data_frame(dataset)
  if (!is_missing(solver) && inherits(solver, "Chat")) {
    solver <- ellmer_chat_to_solver(solver)
  } else {
    check_function(solver)
  }
  check_function(scorer)

  res <- carrier::crate(
    function(...) {
      dataset$output <- character(nrow(dataset))
      dataset$solver <- vector("list", nrow(dataset))
      dataset$score <- logical(nrow(dataset))
      dataset$scorer <- vector("list", nrow(dataset))
      dataset$id <- seq_len(nrow(dataset))

      for (i in seq_len(nrow(dataset))) {
        sample <- dataset[i, , drop = FALSE]

        # execute and log results for the solver
        solver_res <- solver(
          sample$input,
          ...
        )
        dataset$output[i] <- solver_res$result
        dataset$solver[i] <- list(solver_res$chat)

        # execute and log results for the scorer
        scorer_res <- scorer(
          input = sample$input,
          target = sample$target,
          output = dataset$output[i],
          ...
        )
        dataset$score[i] <- scorer_res$result
        dataset$scorer[i] <- list(scorer_res$chat)
      }
    
      dataset
    },
    dataset = dataset,
    solver = solver,
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
  time_start <- Sys.time()
  check_inherits(task, "task")

  result <- task(...)

  eval_log(task = task, ..., result = result, time_start = time_start)

  result
}
