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
#' **The usual flow of LLM evaluation with Tasks calls `$new()` and then `$eval()`.**
#' `$eval()` just calls `$solve()`, `$score()`, `$log()`, and `$view()` in order.
#' The remaining methods are generally only recommended for expert use.
#' 
#' @param solver A function that takes a vector of inputs from the
#' dataset's `input` column as its first argument and determines values 
#' approximating `dataset$target`. Its return value must be a list with 
#' the following elements:
#' 
#' * `result` - A character vector of the final responses, with the same length 
#'   as `dataset$input`.
#' * `solver_chat` - A list of ellmer Chat objects that were used to solve 
#'   each input, also with the same length as `dataset$input`.
#' 
#' Additional output elements can be included in a slot `solver_metadata` that
#' has the same length as `dataset$input`, which will be logged in 
#' `solver_metadata`.
#' 
#' Additional arguments can be passed to the solver via `$solve(...)`
#' or `$eval(...)`. See the definition of [generate()] for a function that
#' outputs a valid solver that just passes inputs to ellmer Chat objects'
#' `$chat()` method in parallel.
#' 
#' @param scorer A function that evaluates how well the solver's return value
#' approximates the corresponding elements of `dataset$target`. The function
#' should take in the `$samples` slot of a Task object and return a list with 
#' the following elements:
#'
#' * `score` - A vector of scores with length equal to `nrow(samples)`. 
#'   Built-in scorers return ordered factors with
#'   levels `I` < `P` (optionally) < `C` (standing for "Incorrect", "Partially
#'   Correct", and "Correct"). If your scorer returns this output type, the
#'   package will automatically calculate metrics.
#' 
#' Optionally: 
#' * `scorer_chat` - If your scorer makes use of ellmer, also include a list of 
#'   ellmer Chat objects that were used to score each result, also with 
#'   length `nrow(samples)`.
#' * `scorer_metadata` - Any intermediate results or other values that you'd
#'   like to be stored in the persistent log. This should also have length 
#'   equal to `nrow(samples)`.
#' 
#' Scorers will probably make use of `samples$input`, `samples$target`, and 
#' `samples$result` specifically. See [model-based scoring][scorer_model] 
#' for examples.
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
#'     solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
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
    #' The typical flow of LLM evaluation with rinspect tends to involve first
    #' calling this method and then `$eval()` on the resulting object.
    #'
    #' @param dataset A tibble with, minimally, columns `input` and `target`.
    #' @param metric A metric summarizing the results from the scorer.
    #' @param name A name for the evaluation task. Defaults to
    #' `deparse(substitute(dataset))`.
    #' @param dir Directory where logs should be stored.
    initialize = function(
      dataset,
      solver,
      scorer,
      metric = NULL,
      epochs = NULL,
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
      check_number_whole(epochs, min = 1, allow_null = TRUE)
      
      private$dataset_name <- gsub("[^[:alnum:]]", "", name)
      self$dir <- dir
      private$solver <- logged(solver, fn_name = solver_name)
      private$scorer <- logged(scorer, fn_name = scorer_name)
      private$task_id <- substr(hash(c(name, solver_name, scorer_name)), 1, 22)
      private$epochs <- epochs

      self$samples <- set_id_column(dataset)
    },

    #' @description
    #' Evaluates the task by running the solver, scorer, logging results, and 
    #' viewing (if interactive). This method works by calling `$solve()`, 
    #' `$score()`, `$log()`, and `$view()` in sequence.
    #' 
    #' The typical flow of LLM evaluation with rinspect tends to involve first
    #' calling `$new()` and then this method on the resulting object.
    #'
    #' @param ... Additional arguments passed to the solver and scorer functions.
    #' @param epochs The number of times to repeat each sample. Evaluate each sample
    #' multiple times to measure variation. Optional, defaults to `1L`.
    #' @param view Automatically open the viewer after evaluation (defaults to
    #' TRUE if interactive, FALSE otherwise).
    #'
    #' @return The Task object (invisibly)
    eval = function(..., epochs = NULL, view = interactive()) {
      check_number_whole(epochs, min = 1, allow_null = TRUE)
      
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
    #' Solve the task by running the solver
    #'
    #' @param epochs The number of times to repeat each sample. Evaluate each sample
    #' multiple times to measure variation. Optional, defaults to `1L`.
    #' @param ... Additional arguments passed to the solver function.
    #'
    #' @return The Task object (invisibly)
    solve = function(..., epochs = NULL) {
      check_number_whole(epochs, min = 1, allow_null = TRUE)
      
      if (private$solved) {
        private$reset_for_new_eval()
      }

      self$samples <- join_epochs(self$samples, epochs %||% private$epochs)

      private$run_id <- generate_id()
      
      self$samples$result <- NA
      self$samples$solver_chat <- NA

      private$solutions <- private$solver(self$samples$input, ...)

      # TODO: it might be nice to just run one of the inputs async and check for
      # this earlier on so that a full eval's worth of results isn't thrown
      # away if the output format isn't quite right.
      private$check_solver_outputs()
      private$cbind_solutions()

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
      private$check_scorer_outputs()
      private$cbind_scores()

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

      eval_log <- eval_log(
        eval = translate_to_eval(
          run_id = private$run_id,
          task = private$dataset_name,
          task_id = private$task_id,
          dataset = list(
            samples = length(unique(samples$id)), 
            sample_ids = as.list(seq_len(length(unique(samples$id)))), 
            shuffled = FALSE
          ),
          model = samples$solver_chat[[1]]$get_model(),
          scorers = translate_to_eval_scorers(
            name = samples$scorer[[1]]
          )
        ),
        plan = translate_to_plan(steps = translate_to_plan_steps(
          name = private$solutions$name,
          arguments = private$solutions$arguments
        )),
        results = translate_to_results(
          total_samples = nrow(samples),
          completed_samples = nrow(samples),
          scores = results_scores(
            name = private$scores$name,
            metrics = map(self$metrics, rename_metric_fields)
          )
        ),
        stats = translate_to_stats(
          started_at = eval_log_timestamp(samples$solver_chat[[1]]$get_turns()[[1]]@completed),
          completed_at = translate_to_completed_at(samples),
          model_usage = sum_model_usage(samples$solver_chat)
        ),
        samples = translate_to_samples(samples, scores = private$scores)
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
    #' Set the solver function
    #' 
    #' @return The Task object (invisibly)
    set_solver = function(solver) {
      solver_name <- deparse(substitute(solver))
      private$solver <- logged(solver, fn_name = solver_name)
      
      if (private$solved) {
        cli::cli_warn("Clearing results from previous solver.")
        private$reset_solutions()
      }
      
      invisible(self)
    },
    
    #' @description
    #' Set the scorer function
    #' 
    #' @return The Task object (invisibly)
    set_scorer = function(scorer) {
      scorer_name <- deparse(substitute(scorer))
      private$scorer <- logged(scorer, fn_name = scorer_name)
      
      if (private$scored) {
        cli::cli_warn("Clearing scores from previous scorer.")
        private$reset_scores()
      }
      
      invisible(self)
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
      self$samples$solver_metadata <- NULL
      private$solved <- FALSE

      if ("epoch" %in% names(self$samples)) {
        self$samples <- self$samples[self$samples$epoch == 1, ]
        self$samples$epoch <- NULL
      }

      invisible(NULL)
    },
    
    reset_scores = function() {
      self$samples$score <- NA
      self$samples$scorer_chat <- NULL
      self$samples$scorer_metadata <- NULL
      private$scored <- FALSE
      invisible(NULL)
    },
    
    reset_for_new_eval = function() {
      private$reset_solutions()
      private$reset_scores()
      invisible(NULL)
    },

    check_solver_outputs = function() {
      if (!all(c("result", "solver_chat") %in% names(private$solutions$value))) {
        cli::cli_abort(
          "{.arg solver} must return slots {.field result} and 
           {.field solver_chat}.",
           call = call2("$solve()")
        )
      }

      first_solver_chat <- private$solutions$value$solver_chat[[1]]
      if (!inherits(first_solver_chat, "Chat")) {
        cli::cli_abort(
          "Elements in the {.field solver_chat} output from {.arg solver} must be
           ellmer Chat objects, not {.obj_type_friendly {first_solver_chat}}.",
           call = call2("$solve()")
        )
      }
    },

    check_scorer_outputs = function() {
      if (!"score" %in% names(private$scores$value)) {
        cli::cli_abort(
          "{.arg scorer} must return a list with (at least) the slot {.field score}.",
           call = call2("$score()")
        )
      }

      if (!"scorer_chat" %in% names(private$scores$value)) {
        return(invisible())
      }

      first_scorer_chat <- private$scores$value$scorer_chat[[1]]
      if (!inherits(first_scorer_chat, "Chat")) {
        cli::cli_abort(
          "Elements in the {.field scorer_chat} output from {.arg scorer} must be
           ellmer Chat objects, not {.obj_type_friendly {first_scorer_chat}}.",
           call = call2("$score()")
        )
      }
    },

    cbind_solutions = function() {
      self$samples$result <- private$solutions$value$result
      self$samples$solver_chat <- private$solutions$value$solver_chat

      if ("solver_metadata" %in% names(private$solutions$value)) {
        self$samples$solver_metadata <- private$solutions$value$solver_metadata
      }

      invisible()
    },

    cbind_scores = function() {
      scorer_res <- private$scores$value
      self$samples$score <- scorer_res$score
      if ("scorer_chat" %in% names(scorer_res)) {
        self$samples$scorer_chat <- scorer_res$scorer_chat
      }
      if ("scorer_metadata" %in% names(scorer_res)) {
        self$samples$scorer_metadata <- scorer_res$scorer_metadata
      }

      self$samples$scorer <- private$scores$name
    },

    # The output of `logged(solver)(...)`
    solutions = NULL,

    # The output of `logged(scorer)(...)`.
    scores = NULL,

    task_id = NULL,
    run_id = NULL,

    epochs = NULL
  )
)

#' @export
#' @importFrom cli cat_line format_inline col_grey
print.Task <- function(x, ...) {
  dataset_name <- x$.__enclos_env__$private$dataset_name

  cli::cat_line(cli::format_inline("An evaluation {cli::col_blue('task')} {.field {dataset_name}}."))

  if ("score" %in% names(x$samples)) {
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
  if (is.null(epochs)) {
    epochs <- 1L
  }
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
