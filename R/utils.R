is_positron <- function() {
  Sys.getenv("POSITRON") == "1"
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

# ad-hoc check functions
check_inherits <- function(x, cls, x_arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, cls)) {
    cli::cli_abort(
      "{.arg {x_arg}} must be a {.cls {cls}}, not {.obj_type_friendly {x}}",
      call = call
    )
  }

  invisible()
}

check_log_dir <- function(x, call = caller_env()) {
  if (is.na(x)) {
    cli::cli_warn(c(
      "!" = "{.pkg vitals} could not find a log directory; evaluation log
             files will be written to a temporary directory.",
      "i" = 'Set a log directory with e.g.
             {.code vitals::vitals_log_dir_set("./logs")}, 
             perhaps in {.file ~/.Rprofile}, to quiet this warning.'
    ))
  }
  invisible(x)
}

# miscellaneous ---------
solver_chat <- function(sample) {
  solver <- sample$solver_chat[[1]]
  solver$clone()$set_turns(list())
}

interactive <- NULL

regenerate_example_objects <- function() {
  source("inst/regenerate-example-objects.R")
}

accuracy <- function(...) {
  mean(...) * 100
}

scrub_provider <- function(chat) {
  old_provider <- chat$get_provider()
  dummy_provider <- ellmer::Provider(
    old_provider@name,
    old_provider@model,
    "example.org"
  )
  chat$.__enclos_env__$private$provider <- dummy_provider
  chat
}

scrub_providers <- function(task) {
  samples <- task$get_samples()

  if ("solver_chat" %in% names(samples)) {
    samples$solver_chat <- purrr::map(samples$solver_chat, scrub_provider)
  }
  if ("scorer_chat" %in% names(samples)) {
    samples$scorer_chat <- purrr::map(samples$scorer_chat, scrub_provider)
  }

  task$.__enclos_env__$private$samples <- samples

  task
}
