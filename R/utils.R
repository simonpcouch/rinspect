is_positron <- function() {
  Sys.getenv("POSITRON") == "1"
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
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
