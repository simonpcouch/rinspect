# Log a function's evaluation
# 
# @description
# Inspect has a logging mechanism s.t. the names of functions called as well 
# as the arguments supplied are stored in the eval log.
# 
# `logged()` takes a function and maps it to a function with this sort of 
# "logged" return value. The function is applied to both internal functions
# and user-supplied solvers/scorers/metrics.
# 
# @returns
# A modified version of `fn()` with signature `...` and a named list return
# value. That list's return value is:
# 
# * `name`: A string representing the name of the function called.
# * `value`: The original return value of `fn()`.
# * `arguments`: Expressions representing arguments passed to `fn()`, 
# captured with [rlang::enexprs()].
# 
# @examples
# logged(mean)(x = 1:3)
# @keywords internal
logged <- function(fn, fn_name = deparse(substitute(fn))) {
  # TODO: `deparse_substitute()` may not be the intended value? may want a 
  # call name instead. we'll see.
  function(...) {
    arg_exprs <- enexprs(...)
    arg_exprs <- map(arg_exprs, expr_deparse)

    res <- list(
      name = fn_name,
      value = fn(...),
      arguments = arg_exprs
    )

    res
  }
}
