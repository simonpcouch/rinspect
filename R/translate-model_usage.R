translate_to_model_usage <- function(chat) {
  tokens <- as.data.frame(chat$get_tokens())
  model <- chat$get_model()

  dots_list(
    !!model := list(
      input_tokens = sum(tokens$tokens[tokens$role == "user"]),
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0,
      output_tokens = sum(tokens$tokens[tokens$role == "assistant"]),
      total_tokens = sum(tokens$tokens)
    )
  )
}
