translate_to_output <- function(turns) {
  last_assistant_turn <- .last_assistant_turn(turns)

  list(
    model = .turn_model(last_assistant_turn),
    choices = translate_assistant_choices(last_assistant_turn),
    usage = translate_to_model_usage(turns),
    # TODO: is this the last time or the beginning time?
    time = attr(last_assistant_turn, "time_completed")
  )
}
