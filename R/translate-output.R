translate_to_output <- function(turns) {
  last_assistant_turn <- .last_assistant_turn(turns)

  list(
    model = .turn_model(last_assistant_turn),
    choices = translate_assistant_choices(last_assistant_turn),
    usage = translate_to_model_usage(turns),
    # TODO: fill this in
    time = 1
  )
}
