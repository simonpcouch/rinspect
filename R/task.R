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

    #' @field metric A named list of metric functions to apply to scoring results.
    metric = NULL,

    #' @field metrics The metrics calculated in `metric()`. 
    metrics = NULL,
    
    #' @description
    #' Set the solver function
    #' 
    #' @param x A solver function
    #' 
    #' @return The Task object (invisibly)
    set_solver = function(x) {
      x_name <- deparse(substitute(x))
      private$solver <- logged(x, fn_name = x_name)
      
      if (private$solved) {
        cli::cli_warn("Clearing results from previous solver.")
        private$reset_solutions()
      }
      
      invisible(self)
    },
    
    #' @description
    #' Set the scorer function
    #' 
    #' @param x A scorer function
    #' 
    #' @return The Task object (invisibly)
    set_scorer = function(x) {
      x_name <- deparse(substitute(x))
      private$scorer <- logged(x, fn_name = x_name)
      
      if (private$scored) {
        cli::cli_warn("Clearing scores from previous scorer.")
        private$reset_scores()
      }
      
      invisible(self)
    },

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
    #' @param metric A metric summarizing the results from the scorer.
    #' @param name A name for the evaluation task. Defaults to
    #' `deparse(substitute(dataset))`.
    #' @param dir Directory where logs should be stored.
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
      private$solver <- logged(solver, fn_name = solver_name)
      private$scorer <- logged(scorer, fn_name = scorer_name)
      private$task_id <- substr(hash(c(name, solver_name, scorer_name)), 1, 22)

      self$samples <- set_id_column(dataset)
    },

    #' @description
    #' Solve the task by running the solver
    #'
    #' @param epochs The number of times to repeat each sample. Evaluate each sample
    #' multiple times to measure variation. Optional, defaults to `1L`.
    #' @param ... Additional arguments passed to the solver function.
    #'
    #' @return The Task object (invisibly)
    solve = function(..., epochs = 1L) {
      check_number_whole(epochs, min = 1)
      
      if (private$solved) {
        private$reset_for_new_eval()
      }

      if (epochs > 1) {
        self$samples <- join_epochs(self$samples, epochs)
      }

      private$run_id <- generate_id()
      
      self$samples$result <- NA
      self$samples$solver_chat <- NA

      private$solutions <- private$solver(as.list(self$samples$input), ...)

      # TODO: it might be nice to just run one of the inputs async and check for
      # this earlier on so that a full eval's worth of results isn't thrown
      # away if the output format isn't quite right.
      private$check_solver_outputs()

      self$samples$result <- private$solutions$value$result
      self$samples$solver_chat <- private$solutions$value$solver_chat
      
      private$solved <- TRUE
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
      if (!private$solved) {
        cli::cli_alert_warning(
          "Task has not been solved yet. Run task$solve() first."
        )
        return(invisible(self))
      }
      
      self$samples$score <- NA
      
      private$scores <- private$scorer(self$samples, ...)
      scorer_res <- private$scores$value
      self$samples$score <- scorer_res$score
      if ("scorer_chat" %in% names(scorer_res)) {
        self$samples$scorer_chat <- scorer_res$scorer_chat
      }
      self$samples$scorer <- private$scores$name
      self$samples$metadata <- scorer_res$metadata

      if (is.factor(self$samples$score) && 
         (any(c("C", "I") %in% levels(self$samples$score)))) {
        # map factor to numeric for a simple accuracy (#51, #53)
        numeric_scores <- as.numeric(self$samples$score) - 1
        numeric_scores <- numeric_scores / max(numeric_scores, na.rm = TRUE)
        self$metrics <- 
          list2(
            accuracy = logged(accuracy)(numeric_scores)
          )
      } else {
        self$metrics <- 
          list2(
            accuracy = logged(accuracy)(self$samples$score)
          )
      }
      
      private$scored <- TRUE
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
      
      if (private$solved || private$scored) {
        private$reset_for_new_eval()
      }

      cli::cli_progress_step("Solving")
      self$solve(..., epochs = epochs)

      cli::cli_progress_step("Scoring")
      self$score(...)
      
      self$log(self$dir)
      private$stash_last_task()

      cli::cli_process_done()
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
      if (!private$solved) {
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
          run_id = private$run_id,
          task = private$dataset_name,
          task_id = private$task_id,
          dataset = list(
            samples = length(unique(samples$id)), 
            sample_ids = seq_len(length(unique(samples$id))), 
            shuffled = FALSE
          ),
          model = .turn_model(.last_assistant_turn(samples$solver_chat[[1]]$get_turns())),
          scorers = eval_log_eval_scorers(
            name = environment(private$scorer)$fn_name
          )
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
            metrics = map(self$metrics, rename_metric_fields)
          )
        ),
        stats = eval_log_stats(
          started_at = eval_log_timestamp(samples$solver_chat[[1]]$get_turns()[[1]]@completed),
          completed_at = eval_log_timestamp(Sys.time()),
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
    
    solver = NULL,
    
    scorer = NULL,
    
    solved = FALSE,
    
    scored = FALSE,

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
    
    reset_solutions = function() {
      self$samples$result <- NA
      self$samples$solver_chat <- NULL
      private$solved <- FALSE
      invisible(NULL)
    },
    
    reset_scores = function() {
      self$samples$score <- NA
      self$samples$scorer_chat <- NULL
      self$samples$metadata <- NULL
      private$scored <- FALSE
      invisible(NULL)
    },
    
    reset_for_new_eval = function() {
      private$reset_solutions()
      private$reset_scores()
      
      if ("epoch" %in% names(self$samples)) {
        self$samples <- self$samples[self$samples$epoch == 1, ]
        self$samples$epoch <- NULL
      }
      invisible(NULL)
    },

    check_solver_outputs = function() {
      if (!all(c("result", "solver_chat") %in% names(private$solutions$value))) {
        cli::cli_abort(
          "{.arg solver} must return slots {.field result} and 
           {.field solver_chat}.",
           call = call2("$solve")
        )
      }

      first_solver_chat <- private$solutions$value$solver_chat[[1]]
      if (!inherits(first_solver_chat, "Chat")) {
        cli::cli_abort(
          "Elements in the {.field solver_chat} output from {.arg solver} must be
           ellmer Chat objects, not {.obj_type_friendly {first_solver_chat}}.",
           call = call2("$solve")
        )
      }
    },

    # The output of `logged(solver)(...)`
    solutions = NULL,

    # The output of `logged(scorer)(...)`.
    scores = NULL,

    task_id = NULL,
    run_id = NULL
  )
)

#' @export
#' @importFrom cli cat_line format_inline col_grey
print.Task <- function(x, ...) {
  dataset_name <- x$.__enclos_env__$private$dataset_name

  cli::cat_line(cli::format_inline("An evaluation {cli::col_blue('task')} {.field {dataset_name}}."))

  if (x$.__enclos_env__$private$scored) {
    cli::cat_line(cli::format_inline(
      "Explore interactively with {.run .last_task$view()}."
    ))
  }

  invisible(x)
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
  metrics$params <- metrics$arguments
  metrics$arguments <- NULL
  metrics
}

rename_token_fields <- function(input_list) {
  name_mapping <- c(
    "input_tokens" = "input_tokens",
    "output_tokens" = "output_tokens", 
    "total_tokens" = "total_tokens",
    "cache_creation_input_tokens" = "input_tokens_cache_write",
    "cache_read_input_tokens" = "input_tokens_cache_read"
  )
  
  result <- list()
  for (name in names(input_list)) {
    if (name %in% names(name_mapping)) {
      result[[name_mapping[name]]] <- input_list[[name]]
    }
  }
  
  result
}
