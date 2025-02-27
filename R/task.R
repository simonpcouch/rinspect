# creation -------------------------------------------------------------------
#' Creating tasks
#'
#' @description
#' Evaluation `tasks` provide a flexible data structure for evaluating LLM-based
#' tools.
#'
#' 1) **Datasets** contain a set of labelled samples. Datasets are just a
#' tibble with columns `input` and `target`, where `input` is a prompt
#' and `target` is either literal value(s) or grading guidance. Situate datasets
#' inside of a task with [task_create()].
#' 2) **Solvers** evaluate the `input` in the dataset and produce a final result.
#' The simplest solver is just an ellmer chat (e.g. [ellmer::chat_claude()]).
#' Evaluate a task with a solver using [task_solve()].
#' 3) **Scorers** evaluate the final output of solvers. They may use text
#' comparisons (like [detect_match()]), model grading (like [model_graded_qa()]),
#' or other custom schemes. Score solver results using [task_score()].
#'
#' @param dataset A tibble with, minimally, columns `input` and `target`.
#' @param name A name for the evaluation task. Defaults to
#' `deparse(substitute(dataset))`.
#' @param dir Directory where logs should be stored.
#'
#' @returns
#' A `task` object, which is a subclass of a tibble.
#' `task_create()` appends column `id` to its input.
#'
#' @seealso
#' A typical evaluation with rinspect calls three functions in sequence:
#' * Create an evaluation task with [task_create()].
#' * Generate solutions with [task_solve()].
#' * Grade solutions with [task_score()].
#'
#' Then, explore task evaluation results in an interactive
#' application using [inspect_view()].
#'
#' @examples
#' if (!identical(Sys.getenv("ANTHROPIC_API_KEY"), "")) {
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
#'   if (interactive()) {
#'     inspect_view(tsk)
#'   }
#' }
#'
#' @export
task_create <- function(
    dataset,
    name = deparse(substitute(dataset)),
    # TODO: maybe it doesn't need to be associated with a dir at all?
    dir = inspect_log_dir()
) {
  force(name)
  check_dataset(dataset)

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

check_dataset <- function(dataset, call = caller_env()) {
  check_data_frame(dataset)
  
  required_cols <- c("input", "target")
  missing_cols <- required_cols[!required_cols %in% names(dataset)]
  
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg dataset} is missing required column{?s} {.field {missing_cols}}.",
      call = call
    )
  }
    
  invisible(dataset)
}

# solving -------------------------------------------------------------------
#' Solving tasks
#'
#' @inherit task_create description
#'
#' @param task An evaluation task created with `task_create()`.
#' @param solver A function that takes an element of `dataset$input` as its first
#' argument and determines a value approximating `dataset$target`.
#' Its return value should be a list with elements `result` (the final response)
#' and `chat` (an ellmer chat used to solve the problem, or a list of them).
#' Or, just supply an ellmer chat (e.g. [ellmer::chat_claude()]).
#' @param epochs The number of times to repeat each sample. Evaluate each sample
#' multiple times to measure variation. Optional, defaults to `1L`.
#'
#' @returns
#' A `task` object, which is a subclass of a tibble.
#' `task_solve()` appends columns `output`, `solver`, and (if not equal to `1L`)
#' `epoch` to its input.
#'
#' @inherit task_create seealso
#' @inherit task_create examples
#' @export
task_solve <- function(task, solver, epochs = 1L) {
  if (inherits(solver, "Chat")) {
    solver <- ellmer_chat_to_solver(solver)
  } else {
    check_function(solver)
  }
  check_inherits(task, "task")
  check_number_whole(epochs, min = 1)

  res <- task_solve_impl(task = task, solver = solver, epochs = epochs)

  task_structure(res)
}

task_solve_impl <- function(task, solver, epochs, ...) {
  task <- join_epochs(task, epochs)

  task$output <- character(nrow(task))
  task$solver <- vector("list", nrow(task))

  withr::local_options(cli.progress_show_after = 0)
  cli::cli_progress_bar("Solving", total = nrow(task))
  for (i in seq_len(nrow(task))) {
    sample <- task[i, , drop = FALSE]

    # execute and log results for the solver
    solver_res <- solver(sample$input)
    task$output[i] <- solver_res$result
    task$solver[i] <- list(solver_res$chat)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  task
}

ellmer_chat_to_solver <- function(chat) {
  carrier::crate(
    function(input) {
      ch <- chat$clone()
      res <- ch$chat(input, echo = FALSE)

      list(result = res, chat = ch)
    },
    chat = chat
  )
}

join_epochs <- function(task, epochs) {
  if (abs(epochs - 1) < .1) {
    return(task)
  }

  dplyr::inner_join(
    task,
    data.frame(
      id = rep(seq_len(nrow(task)), each = epochs),
      epoch = rep(seq_len(nrow(task)), times = epochs)
    ),
    by = "id"
  )
}

# scoring -------------------------------------------------------------------
#' Scoring tasks
#'
#' @inherit task_create description
#'
#' @param scorer A function that evaluates how well the solver's return value
#' approximates the corresponding elements of `dataset$target`. See
#' [model-based scoring][scorer_model] for examples.
#' @inheritParams task_solve
#'
#' @returns
#' A `task` object, which is a subclass of a tibble.
#' `task_score()` appends columns `score` and `scorer` to its input.
#'
#' @inherit task_create seealso
#' @inherit task_create examples
#'
#' @export
task_score <- function(task, scorer) {
  check_inherits(task, "task")
  # TODO: check that it's been solved

  res <- task_score_impl(task, scorer)

  task_structure(res)
}

task_score_impl <- function(task, scorer) {
  task$score <- logical(nrow(task))
  task$scorer <- vector("list", nrow(task))

  withr::local_options(cli.progress_show_after = 0)
  cli::cli_progress_bar("Scoring", total = nrow(task))
  for (i in seq_len(nrow(task))) {
    sample <- task[i, , drop = FALSE]

    # execute and log results for the scorer
    scorer_res <- scorer(sample)
    task$score[i] <- scorer_res$result
    task$scorer[i] <- list(scorer_res$scorer)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  task
}

# helpers -------------------------------------------------------------------
task_structure <- function(x) {
  res <- structure(
    x,
    class = c("task", class(tibble::new_tibble(list())))
  )
  .stash_last_task(res)
  res
}

#' @export
print.task <- function(x, ...) {
  cli::cat_line(cli::format_inline(
    "{cli::col_grey('# Evaluation task')} {.field {(attr(x, 'name'))}}."
  ))

  print(structure(x, class = class(tibble::new_tibble(list()))))

  if (interactive() && has_last_task() && "scorer" %in% colnames(x)) {
    cli::cat_line(cli::format_inline(
      "{cli::col_grey('# View with')} {.run rinspect::inspect_view(.last_task)}."
    ))
  }

  invisible(x)
}

#' Write a task to an eval log
#'
#' @description
#' This function translates a [task][task_create] to an evaluation log file
#' readable by the Inspect log viewer.
#'
#' The usual entry point to this function is [inspect_view()]; use [inspect_log()]
#' to write a persistent log of your task eval. That said, evaluation logs
#' can't be read back into R as `task`s and later analyzed (or viewed in the
#' Inspect log viewer, for that matter); for that use, you likely want to
#' save the `task` using [save()] or [saveRDS()].
#'
#' @inheritParams task_create
#' @inheritParams task_solve
#'
#' @returns
#' The path to the directory which the log was written to. Pass this value
#' to [inspect_view()] to view the task logs.
#'
#' @export
inspect_log <- function(task, dir = attr(res, "dir")) {
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
      started_at = task$solver[[1]]$get_turns()[[1]]@completed,
      completed_at = Sys.time(),
      model_usage = sum_model_usage(task$solver)
    ),
    samples = eval_log_samples(task)
  )

  if (is.na(dir)) {
    dir <- tempdir()
  }
  eval_log_write(eval_log, dir = dir)

  dir
}

# .last_task -------------------------------------------------------------------
.stash_last_task <- function(x) {
  if (!"pkg:rinspect" %in% search()) {
    do.call(
      "attach",
      list(new.env(), pos = length(search()), name = "pkg:rinspect")
    )
  }
  env <- as.environment("pkg:rinspect")
  env$.last_task <- x
  invisible(NULL)
}

has_last_task <- function() {
  if (!"pkg:rinspect" %in% search()) {
    return(FALSE)
  }

  exists(".last_task", as.environment("pkg:rinspect"))
}
