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
