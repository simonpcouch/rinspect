#' Convert a chat to a solver function
#'
#' @description
#' `generate()` is the simplest possible solver one might use with 
#' rinspect; it just passes its inputs to the supplied model and returns
#' its raw responses.
#' 
#' @param solver_chat An ellmer chat object, such as from [ellmer::chat_anthropic()].
#'
#' @returns 
#' The output of `generate()` is another function. That function takes in
#' a vector of `input`s, as well as a solver chat by the 
#' name of `solver_chat` with the default supplied to `generate()` itself,
#' and returns a list of length 2:
#' 
#' * `[[1]]` is the final response from the solver, and
#' * `[[2]]` are the ellmer chat objects that led to that answer.
#' 
#' Both slots should have length equal to the number of `input`s.
#' The `input`s are evaluated in parallel, not in the sense of multiple R
#' sessions, but in the sense of multiple, asynchronous HTTP requests using
#' `$chat_parallel()`.
#' 
#' @export
generate <- function(solver_chat = NULL) {
  chat <- solver_chat
  carrier::crate(
    function(inputs, ..., solver_chat = chat) {
      check_inherits(solver_chat, "Chat")

      ch <- solver_chat$clone()
      res <- ch$chat_parallel(as.list(inputs), max_active = 2, rpm = 20)

      list(
        result = purrr::map_chr(res, function(c) c$last_turn()@text),
        solver_chat = res
      )
    },
    chat = chat,
    check_inherits = check_inherits
  )
}
