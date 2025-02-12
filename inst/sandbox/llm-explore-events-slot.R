# this file explores the "events" slot of a json object converted to an R list.
# we'd like to emulate this data structure from an ellmer chat.

file <-
  system.file(
    "logs/2025-02-08T15-51-00-06-00_simple-arithmetic_o3cKtmsvqQtmXGZhvfDrKB.json",
     package = "rinspect"
  )

example_eval_log <- read_eval_log(file)

# first event --- "sample_init" log the sample, log the user message,
# log the model its sent to
example_eval_log$samples[[1]]$events[[1]]
#> $timestamp
#> [1] "2025-02-08T15:51:00.200266-06:00"
#> 
#> $event
#> [1] "sample_init"
#> 
#> $sample
#> $sample$input
#> [1] "What's 2+2?"
#> 
#> $sample$target
#> [1] "4"
#> 
#> $sample$id
#> [1] 1
#> 
#> 
#> $state
#> $state$messages
#> $state$messages[[1]]
#> $state$messages[[1]]$content
#> [1] "What's 2+2?"
#> 
#> $state$messages[[1]]$source
#> [1] "input"
#> 
#> $state$messages[[1]]$role
#> [1] "user"
#> 
#> 
#> 
#> $state$tools
#> list()
#> 
#> $state$tool_choice
#> NULL
#> 
#> $state$store
#> named list()
#> 
#> $state$output
#> $state$output$model
#> [1] "anthropic/claude-3-5-sonnet-latest"
#> 
#> $state$output$choices
#> list()
#> 
#> 
#> $state$completed
#> [1] FALSE
#> 
#> $state$metadata
#> named list()


# second event --- "step" --- log that the message was sent off
# can safely not write this
example_eval_log$samples[[1]]$events[[2]]
#> [[2]]
#> [[2]]$timestamp
#> [1] "2025-02-08T15:51:00.200599-06:00"
#> 
#> [[2]]$event
#> [1] "step"
#> 
#> [[2]]$action
#> [1] "begin"
#> 
#> [[2]]$type
#> [1] "solver"
#> 
#> [[2]]$name
#> [1] "generate"

# third event --- "model" --- log the model's response and metadata
example_eval_log$samples[[1]]$events[[3]]
#> [[3]]
#> [[3]]$timestamp
#> [1] "2025-02-08T15:51:00.200917-06:00"
#> 
#> [[3]]$event
#> [1] "model"
#> 
#> [[3]]$model
#> [1] "anthropic/claude-3-5-sonnet-latest"
#> 
#> [[3]]$input
#> [[3]]$input[[1]]
#> [[3]]$input[[1]]$content
#> [1] "What's 2+2?"
#> 
#> [[3]]$input[[1]]$source
#> [1] "input"
#> 
#> [[3]]$input[[1]]$role
#> [1] "user"
#> 
#> 
#> 
#> [[3]]$tools
#> list()
#> 
#> [[3]]$tool_choice
#> [1] "none"
#> 
#> [[3]]$config
#> [[3]]$config$max_tokens
#> [1] 4096
#> 
#> 
#> [[3]]$output
#> [[3]]$output$model
#> [1] "claude-3-5-sonnet-20241022"
#> 
#> [[3]]$output$choices
#> [[3]]$output$choices[[1]]
#> [[3]]$output$choices[[1]]$message
#> [[3]]$output$choices[[1]]$message$content
#> [[3]]$output$choices[[1]]$message$content[[1]]
#> [[3]]$output$choices[[1]]$message$content[[1]]$type
#> [1] "text"
#> 
#> [[3]]$output$choices[[1]]$message$content[[1]]$text
#> [1] "2+2=4"
#> 
#> 
#> 
#> [[3]]$output$choices[[1]]$message$source
#> [1] "generate"
#> 
#> [[3]]$output$choices[[1]]$message$role
#> [1] "assistant"
#> 
#> 
#> [[3]]$output$choices[[1]]$stop_reason
#> [1] "stop"
#> 
#> 
#> 
#> [[3]]$output$usage
#> [[3]]$output$usage$input_tokens
#> [1] 14
#> 
#> [[3]]$output$usage$output_tokens
#> [1] 9
#> 
#> [[3]]$output$usage$total_tokens
#> [1] 23
#> 
#> [[3]]$output$usage$input_tokens_cache_write
#> [1] 0
#> 
#> [[3]]$output$usage$input_tokens_cache_read
#> [1] 0
#> 
#> 
#> [[3]]$output$time
#> [1] 0.7982575
#> 
#> 
#> [[3]]$call
#> [[3]]$call$request
#> [[3]]$call$request$messages
#> [[3]]$call$request$messages[[1]]
#> [[3]]$call$request$messages[[1]]$role
#> [1] "user"
#> 
#> [[3]]$call$request$messages[[1]]$content
#> [1] "What's 2+2?"
#> 
#> 
#> 
#> [[3]]$call$request$tools
#> list()
#> 
#> [[3]]$call$request$model
#> [1] "claude-3-5-sonnet-latest"
#> 
#> [[3]]$call$request$max_tokens
#> [1] 4096
#> 
#> 
#> [[3]]$call$response
#> [[3]]$call$response$id
#> [1] "msg_01UTV8iYABmSJn76E5GZTaVQ"
#> 
#> [[3]]$call$response$content
#> [[3]]$call$response$content[[1]]
#> [[3]]$call$response$content[[1]]$citations
#> NULL
#> 
#> [[3]]$call$response$content[[1]]$text
#> [1] "2+2=4"
#> 
#> [[3]]$call$response$content[[1]]$type
#> [1] "text"
#> 
#> 
#> 
#> [[3]]$call$response$model
#> [1] "claude-3-5-sonnet-20241022"
#> 
#> [[3]]$call$response$role
#> [1] "assistant"
#> 
#> [[3]]$call$response$stop_reason
#> [1] "end_turn"
#> 
#> [[3]]$call$response$stop_sequence
#> NULL
#> 
#> [[3]]$call$response$type
#> [1] "message"
#> 
#> [[3]]$call$response$usage
#> [[3]]$call$response$usage$cache_creation_input_tokens
#> [1] 0
#> 
#> [[3]]$call$response$usage$cache_read_input_tokens
#> [1] 0
#> 
#> [[3]]$call$response$usage$input_tokens
#> [1] 14
#> 
#> [[3]]$call$response$usage$output_tokens
#> [1] 9
#> 

# event --- "state" -- can safely not write this
example_eval_log$samples[[1]]$events[[4]]
#> $timestamp
#> [1] "2025-02-08T15:51:00.999886-06:00"
#> 
#> $event
#> [1] "state"
#> 
#> $changes
#> $changes[[1]]
#> $changes[[1]]$op
#> [1] "add"
#> 
#> $changes[[1]]$path
#> [1] "/output/time"
#> 
#> $changes[[1]]$value
#> [1] 0.7982575
#> 
#> 
#> $changes[[2]]
#> $changes[[2]]$op
#> [1] "add"
#> 
#> $changes[[2]]$path
#> [1] "/output/usage"
#> 
#> $changes[[2]]$value
#> $changes[[2]]$value$input_tokens
#> [1] 14
#> 
#> $changes[[2]]$value$output_tokens
#> [1] 9
#> 
#> $changes[[2]]$value$total_tokens
#> [1] 23
#> 
#> $changes[[2]]$value$input_tokens_cache_write
#> [1] 0
#> 
#> $changes[[2]]$value$input_tokens_cache_read
#> [1] 0
#> 
#> 
#> 
#> $changes[[3]]
#> $changes[[3]]$op
#> [1] "replace"
#> 
#> $changes[[3]]$path
#> [1] "/output/model"
#> 
#> $changes[[3]]$value
#> [1] "claude-3-5-sonnet-20241022"
#> 
#> $changes[[3]]$replaced
#> [1] "anthropic/claude-3-5-sonnet-latest"
#> 
#> 
#> $changes[[4]]
#> $changes[[4]]$op
#> [1] "add"
#> 
#> $changes[[4]]$path
#> [1] "/output/choices/0"
#> 
#> $changes[[4]]$value
#> $changes[[4]]$value$message
#> $changes[[4]]$value$message$content
#> $changes[[4]]$value$message$content[[1]]
#> $changes[[4]]$value$message$content[[1]]$type
#> [1] "text"
#> 
#> $changes[[4]]$value$message$content[[1]]$text
#> [1] "2+2=4"
#> 
#> 
#> 
#> $changes[[4]]$value$message$source
#> [1] "generate"
#> 
#> $changes[[4]]$value$message$role
#> [1] "assistant"
#> 
#> 
#> $changes[[4]]$value$stop_reason
#> [1] "stop"
#> 
#> 
#> 
#> $changes[[5]]
#> $changes[[5]]$op
#> [1] "add"
#> 
#> $changes[[5]]$path
#> [1] "/messages/1"
#> 
#> $changes[[5]]$value
#> $changes[[5]]$value$content
#> $changes[[5]]$value$content[[1]]
#> $changes[[5]]$value$content[[1]]$type
#> [1] "text"
#> 
#> $changes[[5]]$value$content[[1]]$text
#> [1] "2+2=4"
#> 
#> 
#> 
#> $changes[[5]]$value$source
#> [1] "generate"
#> 
#> $changes[[5]]$value$role
#> [1] "assistant"


# event --- "step" -- log that the solver completed
example_eval_log$samples[[1]]$events[[5]]
#> $timestamp
#> [1] "2025-02-08T15:51:00.999908-06:00"
#> 
#> $event
#> [1] "step"
#> 
#> $action
#> [1] "end"
#> 
#> $type
#> [1] "solver"
#> 
#> $name
#> [1] "generate"

# event --- "step" -- log that the scorer began
example_eval_log$samples[[1]]$events[[6]]
#> $timestamp
#> [1] "2025-02-08T15:51:00.999929-06:00"
#> 
#> $event
#> [1] "step"
#> 
#> $action
#> [1] "begin"
#> 
#> $type
#> [1] "scorer"
#> 
#> $name
#> [1] "model_graded_qa"

# ...later steps log the solver. can safely ignore for now