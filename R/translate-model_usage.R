translate_to_model_usage <- function(turns) {
  last_assistant_turn <- .last_assistant_turn(turns)
  c(
    last_assistant_turn@json$usage,
    list(total_tokens = sum(unlist(last_assistant_turn@json$usage)))
  )
}
