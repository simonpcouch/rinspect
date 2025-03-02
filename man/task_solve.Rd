% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/task.R
\name{task_solve}
\alias{task_solve}
\title{Solving tasks}
\usage{
task_solve(task, solver, epochs = 1L)
}
\arguments{
\item{task}{An evaluation task created with \code{task_create()}.}

\item{solver}{A function that takes the vector \code{dataset$input} as its first
argument and determines a value approximating \code{dataset$target}.
Its return value should be a list with elements \code{outputs} (a vector of the
final responses, the same length as \code{dataset$input}) and \code{solvers}
(the list of ellmer chats used to solve the inputs, also the same length
as \code{dataset$input}). Or, just supply an ellmer chat
(e.g. \code{\link[ellmer:chat_claude]{ellmer::chat_claude()}}) and rinspect will take care of the details.}

\item{epochs}{The number of times to repeat each sample. Evaluate each sample
multiple times to measure variation. Optional, defaults to \code{1L}.}
}
\value{
A \code{task} object, which is a subclass of a tibble.
\code{task_solve()} appends columns \code{output}, \code{solver}, and (if not equal to \code{1L})
\code{epoch} to its input.
}
\description{
Evaluation \code{tasks} provide a flexible data structure for evaluating LLM-based
tools.
\enumerate{
\item \strong{Datasets} contain a set of labelled samples. Datasets are just a
tibble with columns \code{input} and \code{target}, where \code{input} is a prompt
and \code{target} is either literal value(s) or grading guidance. Situate datasets
inside of a task with \code{\link[=task_create]{task_create()}}.
\item \strong{Solvers} evaluate the \code{input} in the dataset and produce a final result.
The simplest solver is just an ellmer chat (e.g. \code{\link[ellmer:chat_claude]{ellmer::chat_claude()}}).
Evaluate a task with a solver using \code{\link[=task_solve]{task_solve()}}.
\item \strong{Scorers} evaluate the final output of solvers. They may use text
comparisons (like \code{\link[=detect_match]{detect_match()}}), model grading (like \code{\link[=model_graded_qa]{model_graded_qa()}}),
or other custom schemes. Score solver results using \code{\link[=task_score]{task_score()}}.
}
}
\examples{
if (!identical(Sys.getenv("ANTHROPIC_API_KEY"), "")) {
  library(ellmer)
  library(tibble)

  simple_addition <- tibble(
    input = c("What's 2+2?", "What's 2+3?"),
    target = c("4", "5")
  )

  tsk <- task_create(dataset = simple_addition)
  tsk

  tsk <- task_solve(tsk, solver = chat_claude())
  tsk

  tsk <- task_score(tsk, scorer = model_graded_qa())
  tsk

  if (interactive()) {
    inspect_view(tsk)
  }
}

}
\seealso{
A typical evaluation with rinspect calls three functions in sequence:
\itemize{
\item Create an evaluation task with \code{\link[=task_create]{task_create()}}.
\item Generate solutions with \code{\link[=task_solve]{task_solve()}}.
\item Grade solutions with \code{\link[=task_score]{task_score()}}.
}

Then, explore task evaluation results in an interactive
application using \code{\link[=inspect_view]{inspect_view()}}.
}
