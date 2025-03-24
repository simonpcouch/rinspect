translate_to_messages <- function(turns) {
  purrr::map(turns, translate_to_message)
}

translate_to_message <- function(turn) {
  role <- turn@role
  source <- if (role == "user") "input" else "generate"
  
  list(
    id = generate_id(),
    content = if (role == "user") {
      turn@text
    } else {
      list(list(type = "text", text = turn@text))
    },
    source = source,
    role = role
  )
}
