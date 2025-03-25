translate_assistant_choices <- function(turn) {
  list(list(
    message = list(
      id = generate_id(),
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

# translates a Chat object and its corresponding sample to
# Inspect's "event" data structure. Each pair of turns maps to
# something like:
# - Initialization (via the user turn)
#   - step (action: "begin")
#   - sample init (the user turn)
#   - step (action: "end")
# - Solver (same goes, with `sample_init` event being "model")
# - Scorer (same goes, with `sample_init` event being "model")
translate_to_events <- function(sample) {
  solver_chat <- sample$solver_chat[[1]]
  solver_turns <- solver_chat$get_turns()
  solver_turn <- .last_assistant_turn(solver_turns)

  time_user <- solver_turns[[1]]@completed
  time_solver <- solver_turn@completed

  events <- list()
  events <- c(events, create_init_begin_event(time_user))
  events <- c(events, create_sample_init_event(solver_turns[[1]], sample, time_user))
  events <- c(events, create_init_end_event(time_user))
  events <- c(events, create_solver_begin_event(time_user))
  
  events <- c(events, create_model_event(solver_turn, sample, time_solver))
  events <- c(events, create_solver_end_event(time_solver))

  if ("scorer_chat" %in% names(sample)) {
    scorer_chat <- sample$scorer_chat[[1]]
    scorer_turns <- solver_chat$get_turns()
    scorer_turn <- .last_assistant_turn(scorer_turns)
    time_scorer <- .last_assistant_turn(scorer_turns)@completed
    

    events <- c(events, create_scorer_begin_event(time_scorer))
    events <- c(events, create_scoring_model_event(scorer_turn, sample, time_scorer))
    events <- c(events, create_score_event(scorer_turn, sample, time_scorer))
    events <- c(events, create_scorer_end_event(time_scorer))
  }

  events
}

create_init_begin_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "begin",
    name = "init"
  ))
}

create_sample_init_event <- function(turn, sample, timestamp) {
  user_message_id <- generate_id()
  
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "sample_init",
    sample = list(
      input = sample$input,
      target = sample$target,
      id = sample$id
    ),
    state = list(
      messages = list(
        list(
          id = user_message_id,
          content = sample$input,
          source = "input",
          role = "user"
        )
      ),
      tools = list(),
      tool_choice = NULL,
      store = list(),
      output = list(
        # TODO: deduce this from the turn
        model = "claude",
        choices = list()
      ),
      completed = FALSE,
      metadata = c()
    )
  ))
}

create_init_end_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "end",
    name = "init"
  ))
}

create_solver_begin_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "begin",
    type = "solver",
    name = "generate"
  ))
}

create_model_event <- function(turn, sample, timestamp) {
  user_message_id <- generate_id()
  
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "model",
    model = turn@json$model,
    input = list(
      list(
        id = user_message_id,
        content = sample$input,
        source = "input",
        role = "user"
      )
    ),
    tools = list(),
    tool_choice = "none",
    config = list(
      max_tokens = 4096
    ),
    output = list(
      model = turn@json$model,
      choices = list(
        list(
          message = list(
            id = generate_id(),
            content = list(
              list(
                type = "text",
                text = turn@text
              )
            ),
            source = "generate",
            role = "assistant"
          ),
          stop_reason = "stop"
        )
      ),
      usage = rename_token_fields(turn@json$usage),
      time = 100000
    ),
    call = list(
      request = list(
        messages = list(
          list(
            role = "user",
            content = sample$input
          )
        ),
        tools = list(),
        model = turn@json$model,
        max_tokens = 4096,
        extra_headers = list(
          `x-irid` = generate_id()
        )
      ),
      response = list(
        id = paste0("msg_", generate_id()),
        content = list(
          list(
            citations = NULL,
            text = turn@text,
            type = "text"
          )
        ),
        model = turn@json$model,
        role = "assistant",
        stop_reason = "end_turn",
        stop_sequence = NULL,
        type = "message",
        usage = list(
          cache_creation_input_tokens = 0,
          cache_read_input_tokens = 0,
          input_tokens = turn@json$usage$input_tokens,
          output_tokens = turn@json$usage$output_tokens
        )
      ),
      time = 100000
    ),
    completed = events_timestamp(timestamp),
    working_time = 100000
  ))
}

create_solver_end_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "end",
    type = "solver",
    name = "generate"
  ))
}

create_scorer_begin_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "begin",
    type = "scorer",
    name = "model_graded_qa"
  ))
}

create_scoring_model_event <- function(turn, sample, timestamp) {
  user_id <- generate_id()
  
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "model",
    model = turn@json$model,
    input = list(
      list(
        id = user_id,
        content = c(),
        role = "user"
      )
    ),
    tools = list(),
    tool_choice = "none",
    config = list(
      max_tokens = 4096
    ),
    output = list(
      model = turn@json$model,
      choices = list(
        list(
          message = list(
            id = generate_id(),
            content = list(
              list(
                type = "text",
                text = turn@text
              )
            ),
            source = "generate",
            role = "assistant"
          ),
          stop_reason = "stop"
        )
      ),
      usage = rename_token_fields(turn@json$usage),
      time = 100000
    ),
    call = list(
      request = list(
        messages = list(
          list(
            role = "user",
            content = c()
          )
        ),
        tools = list(),
        model = turn@json$model,
        max_tokens = 4096,
        extra_headers = list(
          `x-irid` = generate_id()
        )
      ),
      response = list(
        id = paste0("msg_", generate_id()),
        content = list(
          list(
            citations = NULL,
            text = turn@text,
            type = "text"
          )
        ),
        model = turn@json$model,
        role = "assistant",
        stop_reason = "end_turn",
        stop_sequence = NULL,
        type = "message",
        usage = rename_token_fields(turn@json$usage),
        time = 100000
      )),
    completed = events_timestamp(timestamp),
    working_time = 100000
  ))
}

create_score_event <- function(turn, sample, timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "score",
    score = list(
      value = "C",
      answer = turn@text,
      explanation = turn@text,
      metadata = list(
        grading = list(
          list(
            id = generate_id(),
            content = c(),
            role = "user"
          ),
          list(
            id = generate_id(),
            content = list(
              list(
                type = "text",
                text = turn@text
              )
            ),
            source = "generate",
            role = "assistant"
          )
        )
      )
    ),
    target = sample$target,
    intermediate = FALSE
  ))
}

create_scorer_end_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "end",
    type = "scorer",
    name = "model_graded_qa"
  ))
}

# the events log the timestamp a bit differently than everywhere
# else in the log
events_timestamp <- function(time) {
  sub(pattern = "(\\d{2})(\\d{2})$", replacement = "\\1:\\2", 
    x = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6%z"))
}
