translate_to_messages <- function(chat) {
  turns <- chat$get_turns()
  model <- chat$get_model()
  purrr::map(turns, translate_to_message, model = model)
}

translate_to_message <- function(turn, model) {
  role <- turn@role
  source <- if (role == "user") "input" else "generate"

  message <- list(id = generate_id())

  if (role == "user") {
    if (
      length(turn@contents) == 1 &&
        inherits(turn@contents[[1]], "ellmer::ContentToolResult")
    ) {
      tool_result <- turn@contents[[1]]
      message$content <- tool_result@value %||% as.character(tool_result@error)
      message$tool_call_id <- tool_result@request@id
      message$`function` <- tool_result@request@name
      return(message)
    } else {
      message$content <- turn@text
      message$source <- source
    }
  } else {
    message$content <- list(list(type = "text", text = turn@text))
    message$source <- source

    tool_requests <- purrr::keep(turn@contents, function(content) {
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
      message$model <- model
    }
  }

  message$role <- role

  message
}
