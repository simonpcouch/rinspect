translate_to_model_usage <- function(turns) {
  last_assistant_turn <- .last_assistant_turn(turns)
  model <- .turn_model(last_assistant_turn)

  # TODO: the names of these usage statistics differ slightly between Inspect
  # and what's returned from anthropic's API
  # TODO: this is used in a couple places, and
  # the format that Inspect expects is different
  # in each. for the [["stats"]] slot, this is as
  # it should be. it may not be elsewhere.
  dots_list(
    !!model := c(
      last_assistant_turn@json$usage,
      list(total_tokens = sum(unlist(last_assistant_turn@json$usage)))
    )
  )
}
