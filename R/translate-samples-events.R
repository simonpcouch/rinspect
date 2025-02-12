# TODO: `sample` should actually be the corresponding row of the dataset
# (with an id slot), not the index
translate_to_sample_events <- function(chat, sample = 1L) {
  turns <- chat$get_turns()
  events <- list()
  
  for (i in seq_along(turns)) {
    turn <- turns[[i]]
    if (turn@role == "assistant") {
      events[[i]] <- translate_to_sample_events_assistant(turn)
    } else {
      events[[i]]<- translate_to_sample_events_user(turn, sample)
    }
  }
  
  events
}

translate_to_sample_events_user <- function(turn, sample) {
  list(
    # TODO: grab these from ellmer when available?
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6%z"),
    event = "sample_init",
    # TODO: grab these from the sample when available
    sample = list(
      input = turn@text,
      target = NULL, 
      id = sample
    ),
    state = list(
      messages = list(
        list(
          content = turn@text,
          source = "input",
          role = "user"
        )
      ),
      tools = list(),
      tool_choice = NULL,
      store = list(),
      output = list(
        model = turn@json$model,
        choices = list()
      ),
      completed = FALSE,
      metadata = list()
    )
  )
}

translate_to_sample_events_assistant <- function(turn) {
  list(
    # TODO: grab these from ellmer when available?
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6%z"),
    event = "model",
    model = turn@json$model,
    input = list(list(
      content = turn@text,
      source = "generate",
      role = turn@role
    )),
    tools = list(),
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
    stop_reason = turn@json$stop_reason
  ))
}