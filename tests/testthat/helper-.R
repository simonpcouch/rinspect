example_ellmer_solver <- function() {
  ch <- ellmer::chat_anthropic(
    "Return your answer in the format 'a+b=c', no spaces and no period.",
    model = "claude-3-7-sonnet-latest"
  )
  ch$chat("What's 2+2?", echo = "none")
  ch
}

# a log actually written by Python Inspect
example_inspect_log <- function() {
  eval_log_read(
    system.file(
      "test/inspect/logs/2025-03-24T10-39-36-05-00_simple-arithmetic_fQ9mYnqZFhtEuUenPpJgKL.json",
      package = "vitals"
    )
  )
}

example_task <- function(solved = TRUE, scored = TRUE) {
  simple_addition <- tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  res <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa()
  )

  if (!solved) {
    return(res)
  }

  res$solve()

  if (!scored) {
    return(res)
  }

  res$score()
  res$measure()

  res
}
