# creation -------------------------------------------------------------------
#' Evaluation tasks
#'
#' @description
#' Evaluation `tasks` provide a flexible data structure for evaluating LLM-based
#' tools.
#'
#' 1) **Datasets** contain a set of labelled samples. Datasets are just a
#' tibble with columns `input` and `target`, where `input` is a prompt
#' and `target` is either literal value(s) or grading guidance. Situate datasets
#' inside of a task with `task_create()`.
#' 2) **Solvers** evaluate the `input` in the dataset and produce a final result.
#' The simplest solver is just an ellmer chat---evaluate a task with a solver
#' using `task_solve()`.
#' 3) **Scorers** evaluate the final output of solvers. They may use text
#' comparisons, model grading (like [model_graded_qa()]), or other custom
#' schemes. Score solver results using `task_score()`.
#'
#' @param dataset A tibble with, minimally, columns `input` and `target`.
#' @param solver A function that takes an element of `dataset$input` as input
#' and determines a value approximating `dataset$target`. Its return value should
#' be a list with elements `result` (the final response) and `chat` (an ellmer
#' chat used to solve the problem, or a list of them). Or, just supply an
#' ellmer chat.
#' @param scorer A function that evaluates how well the solver's return value
#' approximates the corresponding elements of `dataset$target`.
#'
#' @returns
#' Each of these functions return a `task`, which is a subclass of a tibble.
#'
#' * `task_create()` appends column `id`.
#' * `task_solve()` appends columns `output` and `solver`.
#' * `task_score()` appends columns `score` and `scorer`.
#'  
#' @seealso [task_log()] and [inspect_view()] for writing results to
#' file and interfacing with the Inspect Log Viewer.
#' 
#' @examples
#' if (interactive()) {
#'   library(ellmer)
#'   library(tibble)
#'
#'   simple_addition <- tibble(
#'     input = c("What's 2+2?", "What's 2+3?"),
#'     target = c("4", "5")
#'   )
#'
#'   tsk <- task_create(dataset = simple_addition)
#'   tsk
#'
#'   tsk <- task_solve(tsk, solver = chat_claude())
#'   tsk
#'
#'   tsk <- task_score(tsk, scorer = model_graded_qa())
#'   tsk
#' 
#'   inspect_view(tsk)
#' }
#'
#' @export
#' @name task
task_create <- function(
    dataset,
    name = deparse(substitute(dataset)),
    # TODO: maybe it doesn't need to be associated with a dir at all?
    dir = eval_log_dir()
) {
  force(name)
  check_data_frame(dataset)
  # TODO: check that it has the appropriate columns

  dataset$id <- seq_len(nrow(dataset))

  res <-
    structure(
      dataset,
      class = c("task", class(tibble::new_tibble(list())))
    )

  attr(res, "name") <- name
  attr(res, "dir") <- dir

  res
}

# solving -------------------------------------------------------------------
#' @export
#' @rdname task
task_solve <- function(task, solver) {
  if (inherits(solver, "Chat")) {
    solver <- ellmer_chat_to_solver(solver)
  } else {
    check_function(solver)
  }

  check_inherits(task, "task")

  res <- task_solve_impl(task = task, solver = solver)

  task_structure(res)
}

task_solve_impl <- function(task, solver, ...) {
  task$output <- character(nrow(task))
  task$solver <- vector("list", nrow(task))

  for (i in seq_len(nrow(task))) {
    sample <- task[i, , drop = FALSE]

    # execute and log results for the solver
    solver_res <- solver(sample$input)
    task$output[i] <- solver_res$result
    task$solver[i] <- list(solver_res$chat)
  }

  task
}

ellmer_chat_to_solver <- function(chat) {
  carrier::crate(
    function(input) {
      ch <- chat$clone()
      res <- ch$chat(input)

      list(result = res, chat = ch)
    },
    chat = chat
  )
}

# scoring -------------------------------------------------------------------
#' @export
#' @rdname task
task_score <- function(task, scorer) {
  check_inherits(task, "task")
  # TODO: check that it's been solved

  res <- task_score_impl(task, scorer)

  task_structure(res)
}

task_score_impl <- function(task, scorer) {
  task$score <- logical(nrow(task))
  task$scorer <- vector("list", nrow(task))

  for (i in seq_len(nrow(task))) {
    sample <- task[i, , drop = FALSE]

    # execute and log results for the scorer
    scorer_res <- scorer(
      input = sample$input,
      target = sample$target,
      output = task$output[i]
    )
    task$score[i] <- scorer_res$result
    task$scorer[i] <- list(scorer_res$chat)
  }

  task
}

# helpers -------------------------------------------------------------------
task_structure <- function(x) {
  structure(
    x,
    class = c("task", class(tibble::new_tibble(list())))
  )
}

#' @export
print.task <- function(x, ...) {
  cli::cat_line(cli::format_inline(
    "{cli::col_grey('# Evaluation task')} {.field {(attr(x, 'name'))}}."
  ))

  print(structure(x, class = class(tibble::new_tibble(list()))))

  if (interactive()) {
    dir <- attr(x, "dir")
    cli::cat_line(cli::format_inline(
      "{cli::col_grey('# View with')} {.run [inspect_view()](rinspect::inspect_view(dir = dir)))}."
    ))
  }

  invisible(x)
}

#' @rdname inspect_view
#' @export
task_log <- function(task, time_start = Sys.time(), dir = attr(res, "dir")) {
  eval_log <- eval_log_new(
    eval = eval_log_eval(
      task = attr(task, "name"),
      dataset = list(samples = nrow(task), sample_ids = seq_len(nrow(task))),
      model = .turn_model(.last_assistant_turn(task$solver[[1]]$get_turns()))
    ),
    results = eval_log_results(
      total_samples = nrow(task),
      completed_samples = nrow(task)
    ),
    # TODO: the wonkiness of the started and completed at times
    # here is a side effect of splitting the solving and scoring up
    # into two. will probably want to take those values independently
    # for those two steps and then subtract out the intermediate time.
    stats = eval_log_stats(
      started_at = time_start,
      completed_at = Sys.time(),
      model_usage = sum_model_usage(task$solver)
    ),
    samples = eval_log_samples(task)
  )

  eval_log_write(eval_log, dir = dir)

  eval_log
}
