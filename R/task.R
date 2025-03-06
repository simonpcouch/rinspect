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
#' @seealso [generate()] for the simplest possible solver, and 
#' [scorer_model] and [scorer_detect] for two built-in approaches to 
#' scoring.
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
#'     solver = generate(chat_claude()),
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
  lock_objects = FALSE,
  public = list(
    #' @field dir The directory where evaluation logs will be written to. Defaults
    #' to `inspect_log_dir()`. 
    dir = inspect_log_dir(),

    #' @field samples A tibble representing the evaluation. Based on the `dataset`,
    #' `epochs` may duplicate rows, and the solver and scorer will append 
    #' columns to this data.
    samples = NULL,
    
    #' @field solver The solver function passed to `$new()`.
    solver = NULL,
    
    #' @field scorer The scorer function passed to `$new()`.
    # TODO: does the setter for this need to be adjusted to apply `logged()`?
    scorer = NULL,

    #' @field metric A named list of metric functions to apply to scoring results.
    metric = NULL,

    #' @field metrics The metrics calculated in `metric()`. 
    metrics = NULL,

    #' @description
    #' Create a new Task object
    #'
    #' @param dataset A tibble with, minimally, columns `input` and `target`.
    #' @param solver A function that takes the vector `dataset$input` as its first
    #' argument and determines a value approximating `dataset$target`.
    #' Its return value should be a list with elements `result` (a vector of the
    #' final responses, the same length as `dataset$input`) and `solver_chat`
    #' (the list of ellmer chats used to solve the inputs, also the same length
    #' as `dataset$input`). See [generate()] for the simplest example.
    #' @param scorer A function that evaluates how well the solver's return value
    #' approximates the corresponding elements of `dataset$target`. See
    #' [model-based scoring][scorer_model] for examples.
    #' @param metric A metric summarizing the results from the scorer. Built-in
    #' scorers are associated with default metrics based on Miller (2024).
    #' @param name A name for the evaluation task. Defaults to
    #' `deparse(substitute(dataset))`.
    #' @param dir Directory where logs should be stored.
    #' 
    #' @source 
    #' "Adding Error Bars to Evals: A Statistical Approach to Language Model 
    #' Evaluations." Miller (2024). https://arxiv.org/pdf/2411.00640
    initialize = function(
      dataset,
      solver,
      scorer,
      metric = NULL,
      name = deparse(substitute(dataset)),
      dir = inspect_log_dir()
    ) {
      force(name)

      solver_name <- deparse(substitute(solver))
      scorer_name <- deparse(substitute(scorer))

      check_dataset(dataset)
      check_log_dir(dir)
      check_function(solver)
      # TODO: for non-built in scorers, what to do?
      check_function(metric, allow_null = TRUE)

      private$dataset_name <- name
      self$dir <- dir
      self$solver <- logged(solver, fn_name = solver_name)
      self$scorer <- logged(scorer, fn_name = scorer_name)

      self$samples <- set_id_column(dataset)
    },

    #' @description
    #' Solve the task by running the solver
    #'
    #' @param ... Additional arguments passed to the solver function.
    #'
    #' @return The Task object (invisibly)
    solve = function(...) {
      private$solutions <- self$solver(as.list(self$samples$input), ...)
      self$samples$result <- private$solutions$value$result
      self$samples$solver_chat <- private$solutions$value$solver_chat
      
      invisible(self)
    },

    #' @description
    #' Score the task by running the scorer and then applying metrics to
    #' its results.
    #'
    #' @param ... Additional arguments passed to the scorer function.
    #'
    #' @return The Task object (invisibly)
    score = function(...) {
      if (!has_result(self$samples)) {
        cli::cli_alert_warning(
          "Task has not been solved yet. Run task$solve() first."
        )
        return(invisible(self))
      }
      
      private$scores <- self$scorer(self$samples, ...)
      scorer_res <- private$scores$value
      self$samples$score <- scorer_res$score
      if ("scorer_chat" %in% names(scorer_res)) {
        self$samples$scorer_chat <- scorer_res$scorer_chat
      }
      self$samples$scorer <- private$scores$name
      self$samples$metadata <- scorer_res$metadata
      
      self$metrics <- 
        list2(
          mean = logged(mean)(self$samples$score),
          standard_error = if ("epoch" %in% names(self$samples)) {
            logged(standard_error)(self$samples$score, cluster = self$samples$id)
          } else {
            logged(standard_error)(self$samples$score)
          }
        )

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
      if (!has_result(self$samples)) {
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
      samples <- self$samples

      eval_log <- eval_log_new(
        eval = eval_log_eval(
          task = private$dataset_name,
          dataset = list(
            samples = length(unique(samples$id)), 
            sample_ids = seq_len(length(unique(samples$id))), 
            shuffled = FALSE
          ),
          model = .turn_model(.last_assistant_turn(samples$solver_chat[[1]]$get_turns())),
        ),
        plan = eval_log_plan(steps = eval_log_plan_steps(
          name = private$solutions$name,
          arguments = private$solutions$arguments
        )),
        results = eval_log_results(
          total_samples = nrow(samples),
          completed_samples = nrow(samples),
          scores = results_scores(
            name = private$scores$name,
            metrics = rename_metric_fields(self$metrics)
          )
        ),
        stats = eval_log_stats(
          started_at = samples$solver_chat[[1]]$get_turns()[[1]]@completed,
          completed_at = Sys.time(),
          model_usage = sum_model_usage(samples$solver_chat)
        ),
        samples = eval_log_samples(samples, scores = private$scores)
      )

      if (is.na(dir)) {
        if (!is.na(self$dir)) {
          dir <- self$dir
        } else {
          dir <- tempdir()
        }
      }

      self$dir <- dir
      eval_log_write(eval_log, dir = dir)

      invisible(self$dir)
    }
  ),

  private = list(
    dataset_name = NULL,

    stash_last_task = function() {
      if (!"pkg:rinspect" %in% search()) {
        do.call(
          "attach",
          list(new.env(), pos = length(search()), name = "pkg:rinspect")
        )
      }
      env <- as.environment("pkg:rinspect")
      env$.last_task <- self
      invisible(NULL)
    },

    # The output of `logged(solver)(...)`
    solutions = NULL,

    # The output of `logged(scorer)(...)`.
    scores = NULL
  )
)

#' @export
#' @importFrom cli cat_line format_inline col_grey
print.Task <- function(x, ...) {
  dataset_name <- x$.__enclos_env__$private$dataset_name

  cli::cat_line(cli::format_inline("An evaluation {cli::col_blue('task')} {.field {dataset_name}}."))

  if (has_scores(x$samples)) {
    cli::cat_line(cli::format_inline(
      "Explore interactively with {.run .last_task$view()}."
    ))
  }

  invisible(x)
}

has_result <- function(samples) {
  "result" %in% names(samples) && length(samples$result) > 0
}

has_scores <- function(samples) {
  "score" %in% names(samples) && length(samples$score) > 0
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

set_id_column <- function(dataset, call = caller_env()) {
  if ("id" %in% names(dataset)) {
    if (any(duplicated(dataset$id))) {
      cli::cli_abort(
        "Duplicated values found in the {.field id} column. Each ID must be unique.",
        call = call
      )
    }
  } else {
    dataset$id <- seq_len(nrow(dataset))
  }
  
  invisible(dataset)
}

join_epochs <- function(samples, epochs) {
  if (abs(epochs - 1) < .1) {
    return(samples)
  }

  dplyr::inner_join(
    samples,
    data.frame(
      id = rep(seq_len(nrow(samples)), each = epochs),
      epoch = rep(seq_len(epochs), times = nrow(samples))
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

rename_metric_fields <- function(metrics) {
  metrics$options <- metrics$arguments
  metrics$arguments <- NULL
  metrics
}
