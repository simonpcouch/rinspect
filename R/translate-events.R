# translates a sample to Inspect's "event" data structure. The high-level
# structure is something like:
# - Initialization (via the user turn)
# - Tool registration (if applicable)
# - Solver
# - Scorer
#
# TODO: how is the tool registered if it's in the scorer?
translate_to_events <- function(sample) {
  events <- translate_events_initialize(sample)
  events <- translate_events_tool_use(events, sample)
  events <- translate_events_solver(events, sample)
  events <- translate_events_scorer(events, sample)
  events
}

# higher-level helpers ------------------------------------------------------
translate_events_initialize <- function(sample) {
  solver_chat <- sample$solver_chat[[1]]
  solver_turns <- solver_chat$get_turns()
  
  time_user <- solver_turns[[1]]@completed
  
  events <- list()
  events <- c(events, create_init_begin_event(time_user))
  events <- c(events, create_sample_init_event(solver_turns[[1]], sample, time_user))
  events <- c(events, create_init_end_event(time_user))
  
  events
}

translate_events_tool_use <- function(events, sample) {
  solver_chat <- sample$solver_chat[[1]]
  solver_turns <- solver_chat$get_turns()
  
  time_user <- solver_turns[[1]]@completed

  if (has_tool_calls(solver_turns)) {
    events <- c(events, create_use_tools_begin_event(time_user))
    events <- c(events, create_tool_state_event(time_user, solver_chat))
    events <- c(events, create_use_tools_end_event(time_user))
  }
  
  events
}

translate_events_solver <- function(events, sample) {
  solver_chat <- sample$solver_chat[[1]]
  solver_turns <- solver_chat$get_turns()
  solver_turn <- solver_chat$last_turn()
  
  time_user <- solver_turns[[1]]@completed
  time_solver <- solver_turn@completed
  
  # From here, the solver logging goes turn-by-turn. For each turn, log
  # the content from that turn as well as the "state" (e.g. previous response
  # history) at that time. Tool calls are logged with a single event, where the
  # "model" event preceding it functions doubly as a user event calling the tool.
  events <- c(events, create_solver_begin_event(time_user))
  
  for (i in seq_along(solver_turns)) {
    if (i == 1) {
      # First turn is the user query, skip it
      next
    }
    
    turn <- solver_turns[[i]]
    
    # For a tool response turn
    if (length(turn@contents) == 1 && inherits(turn@contents[[1]], "ellmer::ContentToolResult")) {
      tool_result <- turn@contents[[1]]
      events <- c(events, create_tool_event(turn, tool_result))
      next
    }
    
    # If we're at the last turn or this is a turn with tool requests
    if (i == length(solver_turns) || 
        any(sapply(turn@contents, function(ct) inherits(ct, "ellmer::ContentToolRequest")))) {
      events <- c(events, create_model_event(turn, sample))
    }
  }
  
  events <- c(events, create_solver_end_event(time_solver))
  
  events
}

translate_events_scorer <- function(events, sample) {
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

# event-specific helpers ------------------------------------------------------
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
        model = sample$solver_chat[[1]]$get_model(),
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

create_use_tools_begin_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "begin",
    type = "solver",
    name = "use_tools"
  ))
}

create_tool_state_event <- function(timestamp, chat) {
  tools_list <- list()
  
  if (length(chat$get_tools()) > 0) {
    tool_defs <- chat$get_tools()
    
    for (i in seq_along(tool_defs)) {
      tool_def <- tool_defs[[i]]
      tool_name <- names(tool_defs)[i]
      
      tool_info <- list(
        op = "add",
        path = paste0("/tools/", i-1),
        value = list(
          name = tool_name,
          description = tool_def@description,
          parameters = list(
            type = "object",
            properties = c(),
            required = list(),
            additionalProperties = FALSE
          )
        )
      )
      
      tools_list <- append(tools_list, list(tool_info))
    }
  }
  
  tools_list <- append(tools_list, list(list(
    op = "replace",
    path = "/tool_choice",
    value = "auto"
  )))
  
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "state",
    changes = tools_list
  ))
}

create_use_tools_end_event <- function(timestamp) {
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "step",
    action = "end",
    type = "solver",
    name = "use_tools"
  ))
}

create_tool_event <- function(turn, tool_result) {
  timestamp <- turn@completed
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "tool",
    type = "function",
    id = tool_result@request@id,
    `function` = tool_result@request@name,
    arguments = tool_result@request@arguments,
    result = tool_result@value,
    events = list(),
    completed = events_timestamp(timestamp),
    working_time = 100000
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

create_model_event <- function(turn, sample) {
  user_message_id <- generate_id()
  solver_chat <- sample$solver_chat[[1]]
  
  turns <- solver_chat$get_turns()
  previous_turns <- list()
  
  for (j in seq_along(turns)) {
    if (identical(turns[[j]], turn)) {
      break
    }
    previous_turns[[length(previous_turns) + 1]] <- turns[[j]]
  }
  
  input_messages <- lapply(previous_turns, function(prev_turn) {
    if (prev_turn@role == "user") {
      if (length(prev_turn@contents) == 1 && 
          inherits(prev_turn@contents[[1]], "ellmer::ContentToolResult")) {
        tool_result <- prev_turn@contents[[1]]
        return(list(
          id = generate_id(),
          content = tool_result@value,
          role = "tool",
          tool_call_id = tool_result@request@id,
          `function` = tool_result@request@name
        ))
      } else {
        return(list(
          id = generate_id(),
          content = prev_turn@text,
          source = "input",
          role = "user"
        ))
      }
    } else {
      message <- list(
        id = generate_id(),
        content = list(list(type = "text", text = prev_turn@text)),
        source = "generate",
        role = "assistant"
      )
      
      tool_requests <- purrr::keep(prev_turn@contents, function(content) {
        inherits(content, "ellmer::ContentToolRequest")
      })
      
      if (length(tool_requests) > 0) {
        tool_calls <- lapply(tool_requests, function(req) {
          list(
            id = req@id,
            `function` = req@name,
            arguments = req@arguments
          )
        })
        
        message$tool_calls <- tool_calls
      }
      
      return(message)
    }
  })
  
  has_tool_calls_in_turn <- any(sapply(turn@contents, function(content) {
    inherits(content, "ellmer::ContentToolRequest")
  }))
  
  tool_calls_list <- list()
  if (has_tool_calls_in_turn) {
    tool_requests <- purrr::keep(turn@contents, function(content) {
      inherits(content, "ellmer::ContentToolRequest")
    })
    
    tool_calls_list <- lapply(tool_requests, function(req) {
      list(
        id = req@id,
        `function` = req@name,
        arguments = req@arguments
      )
    })
  }
  
  stop_reason <- ifelse(has_tool_calls_in_turn, "tool_calls", "stop")
  
  tools_list <- list()
  if (length(solver_chat$get_tools()) > 0) {
    tools <- solver_chat$get_tools()
    tools_list <- lapply(seq_along(tools), function(i) {
      tool <- tools[[i]]
      tool_name <- names(tools)[i]
      
      list(
        name = tool_name,
        description = tool@description,
        parameters = list(
          type = "object",
          properties = c(),
          required = list(),
          additionalProperties = FALSE
        )
      )
    })
  }
  
  output_message <- list(
    id = generate_id(),
    content = list(list(type = "text", text = turn@text)),
    source = "generate",
    role = "assistant"
  )
  
  if (has_tool_calls_in_turn) {
    output_message$tool_calls <- tool_calls_list
  }
  
  output_message$model <- solver_chat$get_model()
  
  request_messages <- lapply(input_messages, function(msg) {
    if (msg$role == "tool") {
      return(list(
        role = "user",
        content = list(list(
          tool_use_id = msg$tool_call_id,
          type = "tool_result",
          content = list(list(type = "text", text = msg$content)),
          is_error = FALSE
        ))
      ))
    } else if (msg$role == "user") {
      return(list(
        role = "user",
        content = list(list(type = "text", text = msg$content))
      ))
    } else if (msg$role == "assistant") {
      if ("tool_calls" %in% names(msg)) {
        tool_use_elements <- lapply(msg$tool_calls, function(tc) {
          list(
            type = "tool_use",
            id = tc$id,
            name = tc$`function`,
            input = tc$arguments
          )
        })
        
        combined_content <- c(
          list(list(type = "text", text = msg$content[[1]]$text)),
          tool_use_elements
        )
        
        return(list(
          role = "assistant",
          content = combined_content
        ))
      } else {
        return(list(
          role = "assistant",
          content = msg$content
        ))
      }
    }
  })
  
  list(list(
    timestamp = events_timestamp(timestamp),
    working_start = 100000,
    event = "model",
    model = solver_chat$get_model(),
    input = input_messages,
    tools = tools_list,
    tool_choice = if (length(tools_list) > 0) "auto" else "none",
    config = list(
      max_tokens = 4096
    ),
    output = list(
      model = solver_chat$get_model(),
      choices = list(
        list(
          message = output_message,
          stop_reason = stop_reason
        )
      ),
      usage = turn_tokens(turn),
      time = 100000
    ),
    call = list(
      request = list(
        messages = request_messages,
        tools = tools_list,
        tool_choice = if (length(tools_list) > 0) list(type = "auto") else "none",
        model = solver_chat$get_model(),
        max_tokens = 4096,
        extra_headers = list(
          `x-irid` = generate_id()
        )
      ),
      response = list(
        id = paste0("msg_", generate_id()),
        content = if (has_tool_calls_in_turn) {
          c(
            list(list(
              citations = NULL,
              text = turn@text,
              type = "text"
            )),
            lapply(tool_calls_list, function(tc) {
              list(
                id = tc$id,
                input = tc$arguments,
                name = tc$`function`,
                type = "tool_use"
              )
            })
          )
        } else {
          list(list(
            citations = NULL,
            text = turn@text,
            type = "text"
          ))
        },
        model = solver_chat$get_model(),
        role = "assistant",
        stop_reason = if (has_tool_calls_in_turn) "tool_use" else "end_turn",
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

# misc helpers -------------------------------------------------------------
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
