test_that("Task R6 class works", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa()
  )

  expect_true(R6::is.R6(tsk))
  expect_true(inherits(tsk, "Task"))
  expect_snapshot(tsk)

  expect_equal(nrow(tsk$get_samples()), nrow(simple_addition))
  expect_named(tsk$get_samples(), c("input", "target", "id"))

  tsk$eval()
  expect_valid_log(tsk$log())
  expect_snapshot(tsk)

  expect_named(
    tsk$get_samples(),
    c(
      "input",
      "target",
      "id",
      "result",
      "solver_chat",
      "score",
      "scorer",
      "scorer_chat",
      "scorer_metadata"
    ),
    ignore.order = TRUE
  )

  expect_equal(tsk, .last_task)
})

test_that("Task with epochs works", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa()
  )

  tsk$eval(epochs = 2)
  expect_valid_log(tsk$log())

  expect_equal(nrow(tsk$get_samples()), nrow(simple_addition) * 2)
  expect_named(
    tsk$get_samples(),
    c(
      "input",
      "target",
      "id",
      "epoch",
      "result",
      "solver_chat",
      "score",
      "scorer",
      "scorer_chat",
      "scorer_metadata"
    ),
    ignore.order = TRUE
  )
})

test_that("Task respects `$new(epochs)`", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa(),
    epochs = 2
  )

  tsk$eval()
  expect_valid_log(tsk$log())

  expect_equal(nrow(tsk$get_samples()), nrow(simple_addition) * 2)
})

test_that("`$eval(epochs)` takes precedence over `$new(epochs)`", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa(),
    epochs = 2
  )

  tsk$eval(epochs = 1)
  expect_valid_log(tsk$log())

  expect_equal(nrow(tsk$get_samples()), nrow(simple_addition))
})

test_that("check_dataset works", {
  expect_snapshot(
    Task$new(
      dataset = data.frame(input = 1),
      solver = function() {
      },
      scorer = function() {
      }
    ),
    error = TRUE
  )
  expect_snapshot(
    Task$new(
      dataset = data.frame(target = 1),
      solver = function() {
      },
      scorer = function() {
      }
    ),
    error = TRUE
  )
  expect_snapshot(
    Task$new(
      dataset = data.frame(x = 1),
      solver = function() {
      },
      scorer = function() {
      }
    ),
    error = TRUE
  )

  d <- data.frame(input = "hey", target = "there")
  expect_equal(d, check_dataset(d))
})

test_that("join_epochs() works", {
  task_data <- data.frame(something = "here", id = 1:3)
  expect_equal(join_epochs(task_data, 1), task_data)

  joined <- join_epochs(task_data, 2)
  expect_equal(nrow(joined), nrow(task_data) * 2)
  expect_equal(joined$epoch, rep(1:2, 3))
  expect_equal(joined$id, rep(1:3, each = 2))
})

test_that("set_id_column works", {
  # no existing `id`
  df <- tibble::tibble(input = c("a", "b"), target = c("c", "d"))
  result <- set_id_column(df)

  expect_equal(nrow(result), 2)
  expect_true("id" %in% names(result))
  expect_equal(result$id, 1:2)

  # existing `id``
  df <- tibble::tibble(input = c("a", "b"), target = c("c", "d"), id = c(5, 10))
  result <- set_id_column(df)

  expect_equal(nrow(result), 2)
  expect_true("id" %in% names(result))
  expect_equal(result$id, c(5, 10))
})

test_that("Task preserves existing id column", {
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))
  local_mocked_bindings(interactive = function(...) FALSE)

  d <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5"),
    id = c(10, 20)
  )

  tsk <- Task$new(
    dataset = d,
    solver = function() {
    },
    scorer = function() {
    }
  )

  expect_equal(tsk$get_samples()$id, c(10, 20))
})

test_that("Task errors informatively with duplicate ids", {
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))

  d <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5"),
    id = c(10, 10)
  )

  expect_snapshot(
    Task$new(
      dataset = d,
      solver = function() {
      },
      scorer = function() {
      }
    ),
    error = TRUE
  )
})

# solver ------------------------------------------------------------------
test_that("set_solver works", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    result = c("4", "5"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = function() {
    },
    scorer = function() {
    }
  )

  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(
        ellmer::chat_openai(model = "gpt-4.1-nano"),
        ellmer::chat_openai(model = "gpt-4.1-nano")
      )
    )
  }
  tsk$set_solver(new_solver)
  tsk$solve()

  expect_equal(tsk$get_samples()$result, c("4", "5"))
  expect_false("solver_metadata" %in% names(tsk$get_samples()))

  # set a new solver that includes metadata
  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(
        ellmer::chat_openai(model = "gpt-4.1-nano"),
        ellmer::chat_openai(model = "gpt-4.1-nano")
      ),
      solver_metadata = c("boop!", "bop!")
    )
  }
  tsk$set_solver(new_solver)
  expect_false(
    any(c("solver_chat", "solver_metadata") %in% names(tsk$get_samples()))
  )
  tsk$solve()

  expect_true("solver_metadata" %in% names(tsk$get_samples()))
})

test_that("set_solver works", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    result = c("4", "5"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = function() {
    },
    scorer = function() {
    }
  )

  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(
        ellmer::chat_openai(model = "gpt-4.1-nano"),
        ellmer::chat_openai(model = "gpt-4.1-nano")
      )
    )
  }
  tsk$set_solver(new_solver)
  tsk$solve()

  expect_equal(tsk$get_samples()$result, c("4", "5"))
  expect_false("solver_metadata" %in% names(tsk$get_samples()))

  # set a new solver that includes metadata
  new_solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(
        ellmer::chat_openai(model = "gpt-4.1-nano"),
        ellmer::chat_openai(model = "gpt-4.1-nano")
      ),
      solver_metadata = c("boop!", "bop!")
    )
  }
  tsk$set_solver(new_solver)
  expect_false(
    any(c("solver_chat", "solver_metadata") %in% names(tsk$get_samples()))
  )
  tsk$solve()

  expect_true("solver_metadata" %in% names(tsk$get_samples()))
})

# scorer ------------------------------------------------------------------
test_that("set_scorer works", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    result = c("4", "5"),
    target = c("4", "5")
  )

  solver <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list(
        ellmer::chat_openai(model = "gpt-4.1-nano"),
        ellmer::chat_openai(model = "gpt-4.1-nano")
      )
    )
  }

  tsk <- Task$new(
    dataset = simple_addition,
    solver = solver,
    scorer = function() {
    }
  )

  tsk$solve()

  # first, return only the score
  scorer_minimal <- function(samples) {
    list(score = c(1, 1))
  }
  tsk$set_scorer(scorer_minimal)
  tsk$score()

  expect_equal(tsk$get_samples()$score, c(1, 1))
  expect_false(any(
    c("scorer_chat", "scorer_metadata") %in% names(tsk$get_samples())
  ))

  # return scorer chats
  scorer_chat <- function(samples) {
    list(
      score = c(1, 1),
      scorer_chat = list(
        ellmer::chat_openai(model = "gpt-4.1-nano"),
        ellmer::chat_openai(model = "gpt-4.1-nano")
      )
    )
  }
  tsk$set_scorer(scorer_chat)
  expect_true(all(is.na(tsk$get_samples()$score)))
  tsk$score()
  expect_true("scorer_chat" %in% names(tsk$get_samples()))

  # return metadata, too
  scorer_metadata <- function(samples) {
    list(
      score = c(1, 1),
      scorer_chat = list(
        ellmer::chat_openai(model = "gpt-4.1-nano"),
        ellmer::chat_openai(model = "gpt-4.1-nano")
      ),
      scorer_metadata = c("beep", "bop")
    )
  }
  tsk$set_scorer(scorer_metadata)
  expect_true(all(is.na(tsk$get_samples()$score)))
  expect_false(any(
    c("scorer_chat", "scorer_metadata") %in% names(tsk$get_samples())
  ))
  tsk$score()
  expect_true(all(
    c("scorer_chat", "scorer_metadata") %in% names(tsk$get_samples())
  ))
})

# metrics ------------------------------------------------------------------
test_that("default metrics are applied effectively", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(ellmer::chat_openai(model = "gpt-4.1-nano")),
    scorer = function(...) {
      list(
        score = factor(c("C", "C"), levels = c("I", "P", "C"))
      )
    }
  )

  tsk$eval()

  expect_equal(tsk$metrics, c("accuracy" = 100))
  expect_valid_log(tsk$log())
})

test_that("task applies non-default metrics", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  # via Task$new()...
  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(ellmer::chat_openai(model = "gpt-4.1-nano")),
    scorer = function(...) {
      list(
        score = factor(c("C", "C"), levels = c("I", "P", "C"))
      )
    },
    metrics = list(pct_correct = function(scores) {
      mean(scores == "C") * 100
    })
  )

  tsk$eval()

  expect_equal(tsk$metrics, c("pct_correct" = 100))
  expect_valid_log(tsk$log())

  # via set_metrics...
  tsk$set_metrics(list(prop_correct = function(scores) {
    mean(scores == "C")
  }))
  expect_null(tsk$metrics)
  tsk$measure()
  expect_equal(tsk$metrics, c("prop_correct" = 1))
})

test_that("task errors informatively with bad metrics", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  # wrong type supplied to `$new()`
  expect_snapshot(
    tsk <- Task$new(
      dataset = simple_addition,
      solver = generate(ellmer::chat_openai(model = "gpt-4.1-nano")),
      scorer = function(...) {
        list(
          score = factor(c("C", "C"), levels = c("I", "P", "C"))
        )
      },
      metrics = function(scores) {
        mean(scores == "C") * 100
      }
    ),
    error = TRUE
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(ellmer::chat_openai(model = "gpt-4.1-nano")),
    scorer = function(...) {
      list(
        score = factor(c("C", "C"), levels = c("I", "P", "C"))
      )
    },
    metrics = list(pct_correct = function(scores) {
      mean(scores == "C") * 100
    })
  )

  # wrong type supplied to `$set_metrics()`
  expect_snapshot(
    tsk$set_metrics(function(...) "boop bop"),
    error = TRUE
  )

  # valid type but bad return type
  expect_snapshot(
    {
      tsk$set_metrics(list(
        bad_metric = function(scores) "this is not a numeric"
      ))
      tsk$eval()
    },
    error = TRUE
  )
})

# misc ------------------------------------------------------------------
test_that("task ids are deterministic", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))

  tsk_1 <-
    Task$new(
      dataset = are,
      solver = generate(),
      scorer = model_graded_qa()
    )

  tsk_2 <-
    Task$new(
      dataset = are,
      solver = generate(),
      scorer = model_graded_qa()
    )

  tsk_id_1 <- tsk_1$.__enclos_env__$private$task_id
  tsk_id_2 <- tsk_2$.__enclos_env__$private$task_id

  expect_equal(tsk_id_1, tsk_id_2)
  expect_equal(nchar(tsk_id_1), 22)
})

test_that("Task completeness is tracked and preserved", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  tmp_dir <- withr::local_tempdir()
  withr::local_envvar(list(VITALS_LOG_DIR = tmp_dir))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  mock_scorer <- function(samples) {
    list(
      score = c(1),
      metadata = list(NULL)
    )
  }

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = mock_scorer
  )

  expect_false(tsk$.__enclos_env__$private$solved)
  expect_false(tsk$.__enclos_env__$private$scored)

  tsk$solve()
  expect_true(tsk$.__enclos_env__$private$solved)

  tsk$score()
  expect_true(tsk$.__enclos_env__$private$scored)

  tsk$set_solver(generate(chat_openai(model = "gpt-4.1-nano")))
  expect_false(tsk$.__enclos_env__$private$solved)

  tsk$solve()
  expect_true(tsk$.__enclos_env__$private$solved)

  tsk$set_scorer(mock_scorer)
  expect_false(tsk$.__enclos_env__$private$scored)

  tsk$solve()
  tsk$score()

  tsk_clone <- tsk$clone()
  original_results <- tsk$get_samples()$result
  original_scores <- tsk$get_samples()$score

  tsk_clone$eval()
  # TODO: expect_valid_log(tsk$log())
  expect_equal(nrow(tsk_clone$get_samples()), nrow(simple_addition))

  expect_equal(tsk$get_samples()$result, original_results)
  expect_equal(tsk$get_samples()$score, original_scores)

  # test re-evaluation with epochs
  tsk_epochs <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = mock_scorer
  )

  tsk_epochs$eval(epochs = 2)
  # TODO: expect_valid_log(tsk$log())
  expect_equal(nrow(tsk_epochs$get_samples()), nrow(simple_addition) * 2)
  expect_true("epoch" %in% names(tsk_epochs$get_samples()))

  tsk_epochs$eval(epochs = 3)
  # TODO: expect_valid_log(tsk$log())
  expect_equal(nrow(tsk_epochs$get_samples()), nrow(simple_addition) * 3)
  expect_true("epoch" %in% names(tsk_epochs$get_samples()))
})

test_that("Task errors informatively with bad solver output", {
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  bad_solver_missing_fields <- function(inputs) {
    list(
      wrong_name = c("4", "5")
      # missing solver_chat
    )
  }

  tsk <- Task$new(
    dataset = simple_addition,
    solver = bad_solver_missing_fields,
    scorer = function() {
    }
  )

  expect_snapshot(tsk$solve(), error = TRUE)
})

test_that("Task detects non-Chat objects in solver_chat", {
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  bad_solver_wrong_type <- function(inputs) {
    list(
      result = c("4", "5"),
      solver_chat = list("not a Chat object", "also not a Chat object")
    )
  }

  tsk <- Task$new(
    dataset = simple_addition,
    solver = bad_solver_wrong_type,
    scorer = function() {
    }
  )

  expect_snapshot(tsk$solve(), error = TRUE)
})

test_that("Task errors informatively with bad scorer output", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = function(samples) {
      list(wrong_name = c("4", "5"))
    }
  )

  expect_snapshot(tsk$eval(), error = TRUE)
})

test_that("Task detects non-Chat objects in scorer_chat", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = function(samples) {
      list(
        score = c("4", "5"),
        scorer_chat = list("not a Chat object", "also not a Chat object")
      )
    }
  )

  expect_snapshot(tsk$eval(), error = TRUE)
})

test_that("token usage is logged correctly", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  # use a couple tokens to ensure non-NULL
  chat_openai(model = "gpt-4.1-nano")$chat("hey!", echo = "none")
  usage_before <- ellmer::token_usage()
  usage_before <- dplyr::filter(usage_before, model == "gpt-4.1-nano")

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa()
  )

  tsk$solve()
  usage_after_solve <- ellmer::token_usage()
  usage_after_solve <- dplyr::filter(usage_after_solve, model == "gpt-4.1-nano")
  cost_after_solve <- tsk$get_cost()
  expect_equal(
    cost_after_solve$input,
    usage_after_solve$input - usage_before$input
  )
  expect_equal(
    cost_after_solve$output,
    usage_after_solve$output - usage_before$output
  )

  tsk$score()
  usage_after_score <- ellmer::token_usage()
  usage_after_score <- dplyr::filter(usage_after_score, model == "gpt-4.1-nano")
  cost_after_score <- tsk$get_cost()
  expect_equal(
    cost_after_score$input,
    usage_after_score$input - usage_before$input
  )
  expect_equal(
    cost_after_score$output,
    usage_after_score$output - usage_before$output
  )
})


test_that("token usage is logged correctly (with unrelated token usage)", {
  skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""))
  skip_if(identical(Sys.getenv("ANTHROPIC_API_KEY"), ""))
  withr::local_envvar(list(VITALS_LOG_DIR = withr::local_tempdir()))
  withr::local_options(cli.default_handler = function(...) {
  })
  local_mocked_bindings(interactive = function(...) FALSE)
  library(ellmer)

  simple_addition <- tibble::tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  # use a couple tokens to ensure non-NULL
  chat_openai(model = "gpt-4.1-nano")$chat("hey!", echo = "none")
  chat_anthropic(model = "claude-3-7-sonnet-latest")$chat("hey!", echo = "none")
  usage_before <- ellmer::token_usage()
  usage_before <- dplyr::filter(usage_before, model == "gpt-4.1-nano")

  tsk <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_openai(model = "gpt-4.1-nano")),
    scorer = model_graded_qa()
  )

  tsk$solve()
  usage_after_solve <- ellmer::token_usage()
  usage_after_solve <- dplyr::filter(usage_after_solve, model == "gpt-4.1-nano")
  cost_after_solve <- tsk$get_cost()
  expect_equal(nrow(cost_after_solve), 1)
  expect_equal(
    cost_after_solve$input,
    usage_after_solve$input - usage_before$input
  )
  expect_equal(
    cost_after_solve$output,
    usage_after_solve$output - usage_before$output
  )
})
