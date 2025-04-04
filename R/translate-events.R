translate_assistant_choices <- function(turn) {
  text_contents <- first_text_contents(turn)
  list(list(
    message = list(
      id = generate_id(),
      content = list(list(
        type = "text",
        text = text_contents@text
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

first_text_contents <- function(turn) {
  turn_contents <- turn@contents
  is_text <- vapply(turn_contents, inherits, logical(1), "ellmer::ContentText")
  turn_contents[[which(is_text)[1]]]
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
  solver_turn <- solver_chat$last_turn()

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
    scorer_turn <- scorer_chat$last_turn()
    time_scorer <- scorer_turn@completed

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
      store = c(),
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
  solver_chat <- sample$solver_chat[[1]]
  
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "model",
    model = solver_chat$get_model(),
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
      model = solver_chat$get_model(),
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
      usage = turn_tokens(turn),
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
        model = solver_chat$get_model(),
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
        model = solver_chat$get_model(),
        role = "assistant",
        stop_reason = "end_turn",
        stop_sequence = NULL,
        type = "message",
        usage = turn_tokens(turn)
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
  scorer_chat <- sample$scorer_chat[[1]]
  
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "model",
    model = scorer_chat$get_model(),
    input = list(
      list(
        id = user_id,
        content = turn@text,
        role = "user"
      )
    ),
    tools = list(),
    tool_choice = "none",
    config = list(
      max_tokens = 4096
    ),
    output = list(
      model = scorer_chat$get_model(),
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
      usage = turn_tokens(turn),
      time = 100000
    ),
    call = list(
      request = list(
        messages = list(
          list(
            role = "user",
            content = turn@text
          )
        ),
        tools = list(),
        model = scorer_chat$get_model(),
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
        model = scorer_chat$get_model(),
        role = "assistant",
        stop_reason = "end_turn",
        stop_sequence = NULL,
        type = "message",
        usage = turn_tokens(turn),
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
            content = turn@text,
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

turn_tokens <- function(turn) {
  tokens_io <- turn@tokens

  list(
    input_tokens = tokens_io[1],
    input_tokens_cache_write = 0,
    input_tokens_cache_read = 0,
    output_tokens = tokens_io[2]
  )
}
