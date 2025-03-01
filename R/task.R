#' Creating and evaluating tasks with R6
#'
#' @description
#' Evaluation `Task`s provide a flexible data structure for evaluating LLM-based
#' tools.
#'
#' 1) **Datasets** contain a set of labelled samples. Datasets are just a
#' tibble with columns `input` and `target`, where `input` is a prompt
#' and `target` is either literal value(s) or grading guidance.
#' 2) **Solvers** evaluate the `input` in the dataset and produce a final result.
#' The simplest solver is just an ellmer chat (e.g. [ellmer::chat_claude()]).
#' 3) **Scorers** evaluate the final output of solvers. They may use text
#' comparisons (like [detect_match()]), model grading (like 
#' [model_graded_qa()]), or other custom schemes.
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
#'     dataset, 
#'     solver = generate(chat = chat_claude()), 
#'     scorer = model_graded_qa()
#'   )
#' 
#'   # evaluate the task (runs solver and scorer)
#'   tsk$eval()
#'   
#'   # view the task results
#'   tsk$view()
#' }
#'
#' @export
Task <- R6::R6Class("Task",
  public = list(
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
      
      if (inherits(solver, "Chat")) {
        solver <- generate(solver)
      } else {
        check_function(solver)
      }
      
      private$dataset_name <- name
      private$log_dir <- dir
      private$solver_fn <- solver
      private$scorer_fn <- scorer
      
      # Initialize internal task tibble
      dataset$id <- seq_len(nrow(dataset))
      private$tbl <- dataset

      invisible(self)
    },
    
    #' @description
    #' Evaluate the task by running the solver and scorer
    #' 
    #' @param ... Additional arguments passed to the solver and scorer functions
    #' @param epochs The number of times to repeat each sample. Evaluate each sample
    #' multiple times to measure variation. Optional, defaults to `1L`.
    #' @param auto_view Automatically open the viewer after evaluation (defaults to 
    #' TRUE if interactive, FALSE otherwise)
    #' 
    #' @return The Task object (invisibly)
    eval = function(..., epochs = 1L, auto_view = interactive()) {
      check_number_whole(epochs, min = 1)
      
      if (epochs > 1) {
        private$tbl <- join_epochs(private$tbl, epochs)
      }
      
      solver_res <- private$solver_fn(as.list(private$tbl$input), ...)
      private$tbl$output <- solver_res$outputs
      private$tbl$solver <- solver_res$solvers
      
      scorer_res <- private$scorer_fn(private$tbl, ...)
      private$tbl$score <- scorer_res$scores
      private$tbl$scorer <- scorer_res$scorer
      private$tbl$metadata <- scorer_res$metadata
      
      private$log()
      private$stash_last_task()

      if (auto_view) {
        self$view()
      }
      
      invisible(self)
    },
    
    #' @description
    #' View the task results in the Inspect log viewer
    #' 
    #' @return The Task object (invisibly)
    view = function() {
      if (!has_output(private$tbl)) {
        cli::cli_alert_warning("Task has not been evaluated yet. Run task$eval() first.")
        return(invisible(self))
      }
      
      inspect_view(private$log_dir)
      invisible(self)
    },
    
    #' @description
    #' Get the internal task tibble
    #' 
    #' @return A tibble with the task data
    data = function() {
      # Return a copy to prevent direct modification
      private$tbl
    }
  ),
  
  private = list(
    tbl = NULL,
    dataset_name = NULL,
    log_dir = NULL,
    solver_fn = NULL,
    scorer_fn = NULL,
    
    log = function() {
      task <- private$tbl
      
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
      
      if (is.na(private$log_dir)) {
        private$log_dir <- tempdir()
      }
      
      private$log_dir <- eval_log_write(eval_log, dir = private$log_dir)
      
      invisible(private$log_dir)
    },
    
    stash_last_task = function() {
      if (!"pkg:rinspect" %in% search()) {
        do.call(
          "attach",
          list(new.env(), pos = length(search()), name = "pkg:rinspect")
        )
      }
      env <- as.environment("pkg:rinspect")
      env$.last_task <- private$tbl
      invisible(NULL)
    }
  )
)

#' @export
#' @importFrom cli cat_line format_inline col_grey
print.Task <- function(x, ...) {
  dataset_name <- x$.__enclos_env__$private$dataset_name
  
  get_fn_expr <- function(fn) {
    if (inherits(fn, "crate")) {
      env <- environment(fn)
      if (exists("chat", env)) {
        return(paste0("generate(chat = ", class(env$chat)[1], "())"))
      } else {
        return("generate()")
      }
    } else {
      fn_name <- deparse(substitute(fn))
      return(fn_name)
    }
  }
  
  solver_expr <- get_fn_expr(x$.__enclos_env__$private$solver_fn)
  scorer_expr <- deparse(substitute(x$.__enclos_env__$private$scorer_fn))
  
  cli::cat_line("An evaluation task.")
  cli::cat_line(cli::format_inline(
    "Dataset: {dataset_name}"
  ))
  cli::cat_line(cli::format_inline(
    "Solver: {solver_expr}"
  ))
  cli::cat_line(cli::format_inline(
    "Scorer: {scorer_expr}"
  ))
  
  task_data <- x$.__enclos_env__$private$tbl
  if (has_output(task_data)) {
    cli::cat_line(cli::format_inline(
      "Status: {cli::col_green('Evaluated')} ({nrow(task_data)} samples)"
    ))
    
    if ("score" %in% names(task_data)) {
      avg_score <- mean(as.numeric(task_data$score), na.rm = TRUE)
      cli::cat_line(cli::format_inline(
        "Average score: {sprintf('%.2f', avg_score)}"
      ))
    }
  } else {
    cli::cat_line(cli::format_inline(
      "Status: {cli::col_yellow('Not evaluated')} ({nrow(task_data)} samples ready)"
    ))
  }
  
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
  carrier::crate(
    function(inputs, ...) {
      ch <- chat$clone()
      res <- ch$chat_parallel(inputs)

      list(
        outputs = purrr::map_chr(res, function(c) c$last_turn()@text),
        solvers = res
      )
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
