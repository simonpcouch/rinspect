# TODO: should there be a default inspect chat?
#' Solver: generate output
#' 
#' Solvers evaluate the `input` in a dataset and produce a result aimed at
#' replicating the `target`. `generate()` is the most basic solver in rinspect, 
#' and just calls the model with the input and collects the output. 
#'
#' @param chat An ellmer chat.
#'
#' @returns
#' A function that takes an `input` parameter and an optional `chat` parameter,
#' returning a named list containing the final `result` as well as the ellmer 
#' `chat` object that generated it.
#'
#' @family solvers
#' @examples
#' generate()
#' @export
generate <- function(chat = ellmer::chat_claude()) {
  ch <- chat

  function(input, chat = ch) {
    ch <- chat$clone()
    res <- ch$chat(input)

    list(result = res, chat = ch)
  }
}
