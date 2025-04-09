eval_log <- function(
    eval = translate_to_eval(),
    plan = translate_to_plan(),
    results = translate_to_results(),
    stats = translate_to_stats(),
    samples
) {
  res <-
    list(
      version = 2L,
      status = "success",
      eval = eval,
      plan = plan,
      results = results,
      stats = stats,
      samples = samples
    )

  structure(res, class = c("eval_log", class(res)))
}

# top-level entries ------------------------------------------------------------
translate_to_eval <- function(
    run_id,
    created = eval_log_timestamp(),
    task,
    task_id,
    task_version = 0,
    task_file = active_file(),
    task_attribs = c(),
    task_args = c(),
    dataset,
    model,
    model_args = c(),
    config = c(),
    # TODO: look into what this actually does
    revision = list(
      type = "git",
      origin = "https://github.com/UKGovernmentBEIS/inspect_ai.git",
      commit = "9140d8a2"
    ),
    packages = list(inspect_ai = "0.3.63"),
    scorers
) {
  list(
    run_id = run_id,
    created = created,
    task = task,
    task_id = task_id,
    task_version = task_version,
    task_file = task_file,
    task_attribs = task_attribs,
    task_args = task_args,
    dataset = dataset,
    model = model,
    model_args = model_args,
    config = config,
    revision = revision,
    packages = packages,
    scorers = scorers
  )
}

translate_to_plan <- function(
    name = "plan",
    steps = translate_to_plan_steps(),
    config = c()
) {
  list(
    name = name,
    steps = steps,
    config = config
  )
}

translate_to_results <- function(
    total_samples,
    completed_samples,
    scores
) {
  list(
    total_samples = total_samples,
    completed_samples = completed_samples,
    scores = scores
  )
}

translate_to_plan_steps <- function(name, arguments) {
  list(list(
    solver = name,
    params = arguments
  ))
}

translate_to_stats <- function(
    started_at,
    completed_at,
    model_usage
) {
  list(
    started_at = started_at,
    completed_at = completed_at,
    model_usage = model_usage
  )
}

translate_to_samples <- function(dataset, scores) {
  res <- list()
  for (i in seq_len(nrow(dataset))) {
    sample <- dataset[i, , drop = FALSE]
    res[[i]] <- translate_to_sample(sample, scores = scores)
  }
  res
}

translate_to_sample <- function(sample, scores) {
  chat <- sample$solver_chat[[1]]
  scorer_name <- scores$name

  turns <- chat$get_turns()
  list(
    id = sample$id,
    epoch = if ("epoch" %in% colnames(sample)) sample$epoch else {1},
    input = sample$input,
    target = sample$target,
    messages = translate_to_messages(chat),
    output = translate_to_output(chat),
    scores = dots_list(
      !!scorer_name := translate_to_score(
        output = sample$result[[1]],
        score = sample$score[[1]],
        scorer = scorer_name,
        scorer_chat = if ("scorer_chat" %in% names(sample)) {
          sample$scorer_chat[[1]]
        } else {
          NULL
        }
      )
    ),
    metadata = c(),
    store = c(),
    events = translate_to_events(sample = sample),
    model_usage = sum_model_usage(list(chat)),
    # TODO: these seem to be prompts passed to the judges
    attachments = c()
  )
} 

# sub-level entries ------------------------------------------------------------
translate_to_metrics <- function(
    name = character(),
    value = numeric(),
    options = list()
) {
  list(
    name = name,
    value = value,
    options = options
  )
}

translate_to_score <- function(output, score, scorer, scorer_chat = NULL) {
  if (is.null(scorer_chat)) {
    return(list(
      value = score,
      answer = output,
      explanation = paste0("Detected correct answer."),
      metadata = c()
    ))
  }

  turns <- scorer_chat$get_turns()
  explanation <- scorer_chat$last_turn()@text
  
  list(
    value = score,
    answer = output,
    explanation = explanation,
    metadata = list(
      grading = lapply(turns, translate_to_metadata_grading)
    )
  )
}

# Inspect formats the content a bit differently depending
# on whether the turn role is user vs. assistant.
translate_to_metadata_grading <- function(turn) {
  if (turn@role == "user") {
    return(
      list(
        id = generate_id(),
        content = turn@text,
        role = "user"
      )
    )
  }

  list(
    id = generate_id(),
    content = list(list(type = "text", text = turn@text)),
    source = "generate", 
    role = turn@role
  )
}

translate_to_eval_scorers <- function(name) {
  list(list(
    name = name,
    options = c(),
    # TODO: make this dynamic once implemented
    metrics = list(
      list(name = "mean", options = c())
    ),
    metadata = c()
  ))
}

# validating log files (for dev use only) --------------------------------------
# invokes Inspect's pydantic models on an eval log file so that
# we can ensure we're writing files that are compatible with the
# viewer.
validate_log <- function(x) {
  if (!file.exists(x)) {
    cli::cli_abort("Log file {x} does not exist.")
  }
  
  py_script <- system.file("test/validate_log.py", package = "rinspect")
  
  if (!file.exists(py_script)) {
    cli::cli_abort("Python validation script {py_script} not found.")
  }
  
  result <- system2(
    "python",
    args = c(py_script, x),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(result, "status")
  
  if (!is.null(status) && status > 0) {
    cli::cli_abort("{result}")
  }
  
  cli::cli_alert_success("Log file validated successfully")
  return(invisible(NULL))
}
