#' Convert a chat to a solver function
#'
#' @description
#' `generate()` is the simplest possible solver one might use with 
#' rinspect; it just passes its inputs to the supplied model and returns
#' its raw responses.
#' 
#' @param chat An ellmer chat object, such as from [ellmer::chat_claude()].
#'
#' @returns 
#' The output of `generate()` is another function. That function takes in
#' a vector of `input`s and returns a list of length 2:
#' 
#' * `[[1]]` is the final response from the solver, and
#' * `[[2]]` are the ellmer chat objects that led to that answer.
#' 
#' The `input`s are evaluated in parallel, not in the sense of multiple R
#' sessions, but in the sense of multiple, asynchronous HTTP requests using
#' `$chat_parallel()`.
#' 
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
