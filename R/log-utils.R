eval_log_new <- function(
    eval = eval_log_eval(),
    plan = eval_log_plan(),
    results = eval_log_results(),
    stats = eval_log_stats(),
    samples,
    reductions = eval_log_reductions()
) {
  res <-
    list(
      version = 2L,
      status = "success",
      eval = eval,
      plan = plan,
      results = results,
      stats = stats,
      samples = samples,
      reductions = reductions
    )

  structure(res, class = c("eval_log", class(res)))
}

# top-level entries ------------------------------------------------------------
eval_log_eval <- function(
    run_id = generate_id(),
    created = eval_log_timestamp(),
    task,
    task_id = generate_id(),
    task_version = 0,
    task_file = active_file(),
    task_attribs = list(),
    task_args = list(),
    dataset,
    model,
    model_args = list(),
    config = list(),
    # TODO: look into what this actually does
    revision = list(
      type = "git",
      origin = "https://github.com/UKGovernmentBEIS/inspect_ai.git",
      commit = "9140d8a2"
    ),
    packages = list(inspect_ai = "0.3.63")
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
    packages = packages
  )
}

eval_log_plan <- function(
    name = "plan",
    steps = list(list(
      solver = "generate",
      params = structure(list(), names = character(0))
    )),
    config = list()
) {
  list(
    name = name,
    steps = steps,
    config = config
  )
}

eval_log_results <- function(
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

eval_log_stats <- function(
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

eval_log_samples <- function(dataset, scores) {
  res <- list()
  for (i in seq_len(nrow(dataset))) {
    sample <- dataset[i, , drop = FALSE]
    res[[i]] <- eval_log_sample(sample, scores = scores)
  }
  res
}

eval_log_sample <- function(sample, scores) {
  chat <- sample$solver_chat[[1]]
  scorer_name <- scores$name

  turns <- chat$get_turns()
  list(
    id = sample$id,
    epoch = if ("epoch" %in% colnames(sample)) sample$epoch else {1},
    input = sample$input,
    target = sample$target,
    messages = translate_to_messages(turns),
    output = translate_to_output(turns),
    scores = dots_list(
      !!scorer_name := eval_log_score(
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
    metadata = list(),
    store = list(),
    events = translate_to_events(chat, sample = sample),
    model_usage = translate_to_model_usage(turns),
    # TODO: these seem to be prompts passed to the judges
    attachments = list()
  )
} 

eval_log_reductions <- function(
    scorer = character(),
    samples = list()
) {
  list(
    list(
      scorer = scorer,
      samples = samples
    )
  )
}

# sub-level entries ------------------------------------------------------------
eval_log_model_usage <- function(
    input_tokens = 1L,
    output_tokens = 1L,
    total_tokens = 2L,
    input_tokens_cache_write = 0L,
    input_tokens_cache_read = 0L
) {
  list(
    input_tokens = input_tokens,
    output_tokens = output_tokens,
    total_tokens = total_tokens,
    input_tokens_cache_write = input_tokens_cache_write,
    input_tokens_cache_read = input_tokens_cache_read
  )
}

eval_log_metrics <- function(
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

eval_log_score <- function(output, score, scorer, scorer_chat = NULL) {
  value <- if (score == 1) {
    "C"
  } else if (score == .5) {
    "P"
  } else {
    "I"
  }
  
  if (is.null(scorer_chat)) {
    return(list(
      value = value,
      answer = output,
      explanation = paste0("Detected correct answer."),
      metadata = list()
    ))
  }

  turns <- scorer_chat$get_turns()
  explanation <- .last_assistant_turn(turns)@text
  
  list(
    value = value,
    answer = output,
    explanation = explanation,
    metadata = list(
      grading = lapply(turns, eval_log_metadata_grading)
    )
  )
}

eval_log_metadata_grading <- function(turn) {
  list(
    content = list(
      list(
        type = "text",
        text = turn@text
      )
    ),
    source = if(turn@role == "user") "input" else "generate", 
    role = turn@role
  )
}

eval_log_timestamp <- function(time = Sys.time()) {
  timestamp <- format(time, "%Y-%m-%dT%H:%M:%S%z")
  gsub("([+-][0-9]{2})([0-9]{2})$", "\\1:\\2", timestamp)
}

generate_id <- function(length = 22) {
  chars <- c(letters, LETTERS, 0:9)
  paste0(sample(chars, length, replace = TRUE), collapse = "")
}

eval_log_filename <- function(eval_log) {
  paste0(
    gsub(":", "-", eval_log$eval$created), "_",
    gsub(" ", "-", gsub("_", "-", eval_log$eval$task)), "_",
    eval_log$eval$task_id, ".json"
  )
}

# given the list of solvers in a dataset, sum across all of their token usage
# TODO: this doesn't work for non-Claude?
sum_model_usage <- function(solvers) {
  chat <- solvers[[1]]
  provider <- chat$.__enclos_env__$private$provider
  if (!inherits(provider, "ellmer::ProviderClaude")) {
    return(list())
  }
  
  usage_per_solver <- lapply(
    solvers,
    function(chat) {translate_to_model_usage(chat$get_turns())[[1]]}
  )
  res <- Reduce(function(x, y) Map(`+`, x, y), usage_per_solver)

  # TODO: ultimately, this needs to be per-model
  dots_list(
    !!.turn_model(.last_assistant_turn(solvers[[1]]$get_turns())) := res
  )
}

active_file <- function() {
  if (!rstudioapi::isAvailable()) {
    return("")
  }
  
  active_document <- rstudioapi::getActiveDocumentContext()
  active_document$path
}

results_scores <- function(name, metrics) {
  list(list(
    name = name,
    scorer = name,
    params = structure(list(), names = character(0)),
    metrics = metrics
  ))
}
