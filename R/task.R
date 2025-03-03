#' Creating and evaluating tasks
#'
#' @description
#' Evaluation `Task`s provide a flexible data structure for evaluating LLM-based
#' tools.
#'
#' 1) **Datasets** contain a set of labelled samples. Datasets are just a
#' tibble with columns `input` and `target`, where `input` is a prompt
#' and `target` is either literal value(s) or grading guidance.
#' 2) **Solvers** evaluate the `input` in the dataset and produce a final result.
#' 3) **Scorers** evaluate the final output of solvers. They may use text
#' comparisons (like [detect_match()]), model grading (like
#' [model_graded_qa()]), or other custom schemes.
#'
#' The usual flow of LLM evaluation with Tasks calls `$new()` and then
#' `$eval()`.
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
#'   # create a new Task
#'   tsk <- Task$new(
#'     dataset = simple_addition,
#'     solver = generate(chat = chat_claude()),
#'     scorer = model_graded_qa()
#'   )
#'
#'   # evaluate the task (runs solver and scorer) and opens
#'   # the results in the Inspect log viewer (if interactive)
#'   tsk$eval()
#' }
#'
#' @export
Task <- R6::R6Class("Task",
  public = list(
    #' @field dir The directory where evaluation logs will be written to. Defaults
    #' to `Sys.getenv("INSPECT_LOG_DIR")`. 
    dir = inspect_log_dir(),

    #' @field samples A tibble representing the evaluation. Based on the `dataset`,
    #' the solver and scorer will append columns to this data.
    samples = NULL,

    #' @description
    #' Create a new Task object
    #'
    #' @param dataset A tibble with, minimally, columns `input` and `target`.
    #' @param solver A function that takes the vector `dataset$input` as its first
    #' argument and determines a value approximating `dataset$target`.
    #' Its return value should be a list with elements `outputs` (a vector of the
    #' final responses, the same length as `dataset$input`) and `solvers`
    #' (the list of ellmer chats used to solve the inputs, also the same length
    #' as `dataset$input`). Or, just supply an ellmer chat
    #' (e.g. [ellmer::chat_claude()]) and rinspect will take care of the details.
    #' @param scorer A function that evaluates how well the solver's return value
    #' approximates the corresponding elements of `dataset$target`. See
    #' [model-based scoring][scorer_model] for examples.
    #' @param name A name for the evaluation task. Defaults to
    #' `deparse(substitute(dataset))`.
    #' @param dir Directory where logs should be stored.
    initialize = function(
      dataset,
      solver,
      scorer,
      name = deparse(substitute(dataset)),
      dir = inspect_log_dir()
    ) {
      force(name)
      check_dataset(dataset)
      solver_name <- deparse(substitute(solver))
      scorer_name <- deparse(substitute(scorer))

      if (inherits(solver, "Chat")) {
        solver <- generate(solver)
      } else {
        check_function(solver)
      }

      private$dataset_name <- name
      private$solver_name <- solver_name
      private$scorer_name <- scorer_name
      self$dir <- dir
      private$solver_fn <- solver
      private$scorer_fn <- scorer

      dataset$id <- seq_len(nrow(dataset))
      self$samples <- dataset
    },

    #' @description
    #' Solve the task by running the solver
    #'
    #' @param ... Additional arguments passed to the solver function.
    #'
    #' @return The Task object (invisibly)
    solve = function(...) {
      solver_res <- private$solver_fn(as.list(self$samples$input), ...)
      self$samples$output <- solver_res$outputs
      self$samples$solver <- solver_res$solvers
      
      invisible(self)
    },

    #' @description
    #' Score the task by running the scorer
    #'
    #' @param ... Additional arguments passed to the scorer function.
    #'
    #' @return The Task object (invisibly)
    score = function(...) {
      if (!has_output(self$samples)) {
        cli::cli_alert_warning(
          "Task has not been solved yet. Run task$solve() first."
        )
        return(invisible(self))
      }
      
      scorer_res <- private$scorer_fn(self$samples, ...)
      self$samples$score <- scorer_res$scores
      self$samples$scorer <- scorer_res$scorer
      self$samples$metadata <- scorer_res$metadata
      
      invisible(self)
    },

    #' @description
    #' Evaluate the task by running the solver, scorer, logging results, and viewing (if interactive)
    #'
    #' This method works by calling `$solve()`, `$score()`, `$log()`, and `$view()` in sequence.
    #'
    #' @param ... Additional arguments passed to the solver and scorer functions.
    #' @param epochs The number of times to repeat each sample. Evaluate each sample
    #' multiple times to measure variation. Optional, defaults to `1L`.
    #' @param view Automatically open the viewer after evaluation (defaults to
    #' TRUE if interactive, FALSE otherwise).
    #'
    #' @return The Task object (invisibly)
    eval = function(..., epochs = 1L, view = interactive()) {
      check_number_whole(epochs, min = 1)

      if (epochs > 1) {
        self$samples <- join_epochs(self$samples, epochs)
      }

      self$solve(...)
      self$score(...)
      
      self$log(self$dir)
      private$stash_last_task()

      if (view) {
        self$view()
      }

      invisible(self)
    },

    #' @description
    #' View the task results in the Inspect log viewer
    #'
    #' @return The Task object (invisibly)
    view = function() {
      if (!has_output(self$samples)) {
        cli::cli_alert_warning(
          "Task has not been evaluated yet. Run task$eval() first."
        )
        return(invisible(self))
      }

      inspect_view(self$dir)
      invisible(self)
    },

    #' @description
    #' Log the task to a directory.
    #'
    #' Note that, if an `INSPECT_LOG_DIR` envvar is set, this will happen
    #' automatically in `$eval()`.
    #'
    #' @param dir The directory to write the log to.
    #'
    #' @return The path to the logged file, invisibly.
    log = function(dir = inspect_log_dir()) {
      task <- self$samples

      eval_log <- eval_log_new(
        eval = eval_log_eval(
          task = private$dataset_name,
          dataset = list(samples = nrow(task), sample_ids = seq_len(nrow(task))),
          model = .turn_model(.last_assistant_turn(task$solver[[1]]$get_turns()))
        ),
        results = eval_log_results(
          total_samples = nrow(task),
          completed_samples = nrow(task)
        ),
        stats = eval_log_stats(
          started_at = task$solver[[1]]$get_turns()[[1]]@completed,
          completed_at = Sys.time(),
          model_usage = sum_model_usage(task$solver)
        ),
        samples = eval_log_samples(task)
      )

      if (is.na(dir)) {
        if (!is.na(self$dir)) {
          dir <- self$dir
        } else {
          dir <- tempdir()
        }
      }

      eval_log_write(eval_log, dir = dir)

      # TODO: actually return the file path rather than log dir
      invisible(self$dir)
    }
  ),

  private = list(
    dataset_name = NULL,
    solver_name = NULL,
    scorer_name = NULL,
    solver_fn = NULL,
    scorer_fn = NULL,

    stash_last_task = function() {
      if (!"pkg:rinspect" %in% search()) {
        do.call(
          "attach",
          list(new.env(), pos = length(search()), name = "pkg:rinspect")
        )
      }
      env <- as.environment("pkg:rinspect")
      env$.last_task <- self$samples
      invisible(NULL)
    }
  )
)

#' @export
#' @importFrom cli cat_line format_inline col_grey
print.Task <- function(x, ...) {
  dataset_name <- x$.__enclos_env__$private$dataset_name
  solver_name <- x$.__enclos_env__$private$solver_name
  scorer_name <- x$.__enclos_env__$private$scorer_name

  cli::cat_line(cli::format_inline("An evaluation {cli::col_blue('task')}."))
  cli::cat_line(cli::format_inline("Dataset: {.field {dataset_name}}"))
  cli::cat_line(cli::format_inline("Solver: {.field {solver_name}}"))
  cli::cat_line(cli::format_inline("Scorer: {.field {scorer_name}}"))

  invisible(x)
}

has_output <- function(task) {
  "output" %in% names(task) && length(task$output) > 0
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

#' Convert a chat to a solver function
#'
#' @param chat An ellmer chat object, such as from [ellmer::chat_claude()]
#'
#' @return A solver function that can be used with Task
#' @export
generate <- function(chat) {
  ch <- chat
  carrier::crate(
    function(inputs, ..., chat = ch) {
      ch <- chat$clone()
      res <- ch$chat_parallel(inputs)

      list(
        outputs = purrr::map_chr(res, function(c) c$last_turn()@text),
        solvers = res
      )
    },
    ch = ch
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
      epoch = rep(seq_len(epochs), times = nrow(task))
    ),
    by = "id"
  )
}

has_last_task <- function() {
  if (!"pkg:rinspect" %in% search()) {
    return(FALSE)
  }

  exists(".last_task", as.environment("pkg:rinspect"))
}
