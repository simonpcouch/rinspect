translate_to_output <- function(chat) {
  last_assistant_turn <- chat$last_turn()

  list(
    model = chat$get_model(),
    choices = translate_assistant_choices(last_assistant_turn),
    usage = rename_token_fields(translate_to_model_usage(chat)[[1]]),
    # TODO: is this the last time or the beginning time in Inspect?
    # Used to be `last_assistant_turn@completed`, but Inspect gives error:
    # samples.1.output.time: Input should be a valid number, unable to parse string as a number [type=float_parsing, input_value='2025-03-24 09:18:45', input_type=str] 
    time = 1
  )
}
