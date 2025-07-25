% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind.R
\name{vitals_bind}
\alias{vitals_bind}
\title{Concatenate task samples for analysis}
\usage{
vitals_bind(...)
}
\arguments{
\item{...}{\code{Task} objects to combine, optionally named.}
}
\value{
A tibble with the combined samples from all tasks, with a \code{task}
column indicating the source and a nested \code{metadata} column containing
additional fields.
}
\description{
Combine multiple \link{Task} objects into a single tibble for comparison.

This function takes multiple (optionally named) \link{Task} objects and row-binds
their \verb{$get_samples()} together, adding a \code{task} column to identify the source of each
row. The resulting tibble nests additional columns into a \code{metadata} column
and is ready for further analysis.
}
\examples{
if (!identical(Sys.getenv("ANTHROPIC_API_KEY"), "")) {
  # set the log directory to a temporary directory
  withr::local_envvar(VITALS_LOG_DIR = withr::local_tempdir())

  library(ellmer)
  library(tibble)

  simple_addition <- tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk1 <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = model_graded_qa()
  )
  tsk1$eval()

  tsk2 <- Task$new(
    dataset = simple_addition,
    solver = generate(chat_anthropic(model = "claude-3-7-sonnet-latest")),
    scorer = detect_includes()
  )
  tsk2$eval()

  combined <- vitals_bind(model_graded = tsk1, string_detection = tsk2)
}

}
