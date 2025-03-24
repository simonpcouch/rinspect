translate_to_events <- function(chat, sample) {
  turns <- chat$get_turns()
  events <- list()
  
  for (i in seq_along(turns)) {
    turn <- turns[[i]]
    if (turn@role == "assistant") {
      events[[i]] <- translate_to_events_assistant(turn)
    } else {
      events[[i]]<- translate_to_events_user(turn, sample)
    }
  }
  
  events
}

translate_to_events_user <- function(turn, sample) {
  list(
    sample = list(
      input = sample$input,
      target = sample$target, 
      id = sample$id
    ),
    timestamp = turn@completed,
    event = "sample_init",
    state = list(
      messages = list(
        list(
          content = turn@text,
          source = "input",
          role = "user"
        )
      ),
      tools = c(),
      tool_choice = NULL,
      store = c(),
      output = list(
        model = turn@json$model,
        choices = list()
      ),
      completed = FALSE,
      metadata = list()
    )
  )
}

translate_to_events_assistant <- function(turn) {
  list(
    timestamp = turn@completed,
    event = "model",
    model = turn@json$model,
    input = list(list(
      content = turn@text,
      source = "generate",
      role = turn@role
    )),
    tools = c(),
    tool_choice = "none",
    config = list(max_tokens = NA),
    output = list(
      model = turn@json$model,
      choices = translate_assistant_choices(turn),
      usage = c(
        turn@json$usage,
        list(total_tokens = sum(unlist(turn@json$usage)))
      ),
      time = 0.8
    )
  )
}

translate_assistant_choices <- function(turn) {
  list(list(
    message = list(
      content = list(list(
        type = turn@json$content[[1]]$type,
        text = turn@json$content[[1]]$text
      )),
        source = "generate", 
      role = turn@role
    ),
    # turn@json$stop_reason gives the actual stop reason
    # (often 'end_turn'), but
    # Inspect requires "Input should be 'stop', 'max_tokens', 'model_length', 'tool_calls', 'content_filter' or 'unknown' " . (#7)
    stop_reason = "stop"
  ))
}
