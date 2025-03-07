#' Compare two task evaluations
#'
#' @description
#' Compares performance metrics between two evaluations and calculates 
#' statistical measures of their differences.
#' 
#' @param x,y [Task] objects arising from the same call to `Task$new()`.
#' @param conf_level Optional. A single numeric value between 0 and 1 
#' representing the confidence level. Defaults to 0.95.
#'
#' @details
#' The statistics reported here are based on Section 4, _Comparing models_ in
#'  "Adding Error Bars to Evals: A Statistical Approach to Language Model 
#' Evaluations." Miller (2024), <https://arxiv.org/abs/2411.00640>.
#' 
#' Notably, confidence intervals are based on Equations (7) and (5) with
#' no clustering variable, and Equation (8) with clustering variables.
#' Epochs are treated as clustering variables, i.e. multiple samples based on
#' a given question are considered clustered.
#' 
#' @returns 
#' A list containing statistics comparing the two task results
#' 
#' * `mean_x`: The mean score of the first task evaluation (`x`).
#' * `mean_y`: The mean score of the second task evaluation (`y`).
#' * `mean_diff`: The mean difference between scores (`x` - `y`).
#' * `paired_se`: The standard error of the paired differences.
#' * `conf_int`: A vector containing the lower and upper bounds of the 
#' confidence interval for the mean difference.
#'
#' The function will error if the tasks have different 
#' `task_id`s or different numbers of samples.
#'
#' @examples
# load("vignettes/data/are_task.rda")
# load("vignettes/data/are_task_openai.rda")
# inspect_compare(are_task, are_task_openai)
#' if (!identical(Sys.getenv("ANTHROPIC_API_KEY"), "") &&
#'     !identical(Sys.getenv("OPENAI_API_KEY"), "")) {
#'   library(ellmer)
#'   library(tibble)
#'
#'   simple_addition <- tibble(
#'     input = c("What's 2+2?", "What's 2+3?"),
#'     target = c("4", "5")
#'   )
#'
#'   # notably, both of the evaluations passed to `inspect_compare()`
#'   # will be based on the same call to `Task$new()`
#'   tsk <- Task$new(
#'     dataset = simple_addition,
#'     solver = generate(),
#'     scorer = model_graded_qa()
#'   )
#'
#'   tsk_claude <- tsk$clone()
#'   tsk_claude$eval(solver_chat = chat_claude())
#' 
#'   tsk_openai <- tsk$clone()
#'   tsk_openai$eval(solver_chat = chat_openai())
#' 
#'   # do note that this is a rather silly comparison :)
#'   inspect_compare(tsk_claude, tsk_openai)
#' }
#' @export
inspect_compare <- function(x, y, conf_level = .95) {
  check_inherits(x, "Task")
  check_inherits(y, "Task")
  check_number_decimal(conf_level, min = 0, max = 1)

  if (!identical(
    x$.__enclos_env__$private$task_id,
    y$.__enclos_env__$private$task_id
  )) {
    cli::cli_abort(
      "{.arg x} and {.arg y} must be based on the same evaluation task
       (i.e. result from the same call to `Task$new()`)."
    )
  }

  if (nrow(x$samples) != nrow(y$samples)) {
    cli::cli_abort(
      "{.arg x} and {.arg y} must have the same number of samples."
    )
  }

  if ("epoch" %in% colnames(x$samples)) {
    return(paired_comparison_clustered(x, y, conf_level = conf_level))
  }

  paired_comparison(x, y, conf_level = conf_level)
}

paired_comparison <- function(x, y, conf_level) {
  scores_x <- x$samples$score
  scores_y <- y$samples$score
  scores_diff <- scores_x - scores_y

  n <- length(scores_x)

  mean_x <- mean(scores_x)
  mean_y <- mean(scores_y)
  mean_diff <- mean(scores_diff)

  paired_se <- sqrt(var(scores_diff) / n)
  z <- mean_diff / paired_se

  z_crit <- qnorm((1 + conf_level) / 2)
  conf_int <- c(mean_diff - z_crit * paired_se, mean_diff + z_crit * paired_se)

  list(
    mean_x = mean_x,
    mean_y = mean_y,
    mean_diff = mean_diff,
    paired_se = paired_se,
    conf_int = conf_int
  )
}

paired_comparison_clustered <- function(x, y, conf_level) {
  scores_x <- x$samples$score
  scores_y <- y$samples$score
  scores_diff <- scores_x - scores_y
  clusters <- x$samples$id
  n <- length(scores_diff)
  
  mean_x <- mean(scores_x)
  mean_y <- mean(scores_y)
  mean_diff <- mean(scores_diff)
  
  clusters <- as.factor(clusters)
  
  data <- data.frame(diff = scores_diff, cluster = clusters)
  
  sum_clustered <- 0
  
  for (c in levels(clusters)) {
    cluster_diffs <- data$diff[data$cluster == c]
    
    if (length(cluster_diffs) <= 1) {
      next
    }
    
    deviations <- cluster_diffs - mean_diff
    
    for (i in 1:(length(deviations) - 1)) {
      for (j in (i + 1):length(deviations)) {
        sum_clustered <- sum_clustered + deviations[i] * deviations[j]
      }
    }
  }
  
  paired_se <- sqrt(sum_clustered) / n
  z <- mean_diff / paired_se
  
  z_crit <- qnorm((1 + conf_level) / 2)
  conf_int <- c(mean_diff - z_crit * paired_se, mean_diff + z_crit * paired_se)
  
  list(
    mean_x = mean_x,
    mean_y = mean_y,
    mean_diff = mean_diff,
    paired_se = paired_se,
    conf_int = conf_int
  )
}
