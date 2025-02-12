translate_model_usage <- function(turns) {
  last_assistant_turn <- last_assistant_turn(turns)
  model <- last_assistant_turn@json$model
  
  # TODO: the names of these usage statistics differ slightly between Inspect
  # and what's returned from anthropic's API
  dots_list(
    !!model := c(
      last_assistant_turn@json$usage,
      list(total_tokens = sum(unlist(last_assistant_turn@json$usage)))
    )
  )
}
