
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rinspect

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rinspect)](https://CRAN.R-project.org/package=rinspect)
[![R-CMD-check](https://github.com/simonpcouch/rinspect/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simonpcouch/rinspect/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

rinspect is a framework for large language model evaluation in R. It’s
specifically aimed at ellmer users who want to measure the effectiveness
of their LLM-based apps.

The package is an R port of the widely adopted Python framework
[Inspect](https://inspect.ai-safety-institute.org.uk/). While the
package doesn’t integrate with Inspect directly, it allows users to
interface with the [Inspect Log
Viewer](https://inspect.ai-safety-institute.org.uk/log-viewer.html) and
provides an on-ramp to transition to Inspect if need be by writing
evaluation logs to the same file format.

> **Important**
>
> 🚧 Under construction! 🚧
>
> rinspect is highly experimental and much of its documentation is
> aspirational.

## Installation

You can install the developmental version of rinspect using:

``` r
pak::pak("simonpcouch/rinspect")
```

## Example

Inspect is based on three core concepts: datasets, solvers, and scorers.
In rinspect, each of these objects act on **tasks**.

``` r
library(rinspect)
library(ellmer)
library(tibble)
```

**Datasets** are a set of labelled samples where `input` is a prompt and
`target` is either literal value(s) or grading guidance. Situate a
dataset inside of a task with `task_create()`:

``` r
simple_addition <- tibble(
  input = c("What's 2+2?", "What's 2+3?", "What's 2+4?"),
  target = c("4", "5", "6")
)

tsk <- task_create(dataset = simple_addition)
tsk
#> # Evaluation task simple_addition.
#> # A tibble: 3 × 3
#>   input       target    id
#> * <chr>       <chr>  <int>
#> 1 What's 2+2? 4          1
#> 2 What's 2+3? 5          2
#> 3 What's 2+4? 6          3
```

**Solvers** evaluate the `input` in the dataset and produce a final
result (hopefully) resembling `target`. The simplest example of a solver
is a plain ellmer chat—evaluate a solver on a task with `task_solve()`:

``` r
tsk <- task_solve(tsk, solver = chat_claude())
#> Solving ■■■■■■■■■■■                       33% | ETA:  2s
#> Solving ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  1s
#> Solving ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
tsk
#> # Evaluation task simple_addition.
#> # A tibble: 3 × 5
#>   input       target    id output  solver
#> * <chr>       <chr>  <int> <chr>   <list>
#> 1 What's 2+2? 4          1 2+2=4   <Chat>
#> 2 What's 2+3? 5          2 2+3 = 5 <Chat>
#> 3 What's 2+4? 6          3 2+4 = 6 <Chat>
```

**Scorers** evaluate the final output of solvers. They may use text
comparisons, model grading, or other custom schemes. Score solver output
using `task_score()`:

``` r
tsk <- task_score(tsk, scorer = model_graded_qa())
#> Scoring ■■■■■■■■■■■                       33% | ETA:  5s
#> Scoring ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  3s
#> Scoring ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
tsk
#> # Evaluation task simple_addition.
#> # A tibble: 3 × 7
#>   input       target    id output  solver score scorer
#> * <chr>       <chr>  <int> <chr>   <list> <dbl> <list>
#> 1 What's 2+2? 4          1 2+2=4   <Chat>     1 <Chat>
#> 2 What's 2+3? 5          2 2+3 = 5 <Chat>     1 <Chat>
#> 3 What's 2+4? 6          3 2+4 = 6 <Chat>     1 <Chat>
```

Once a task has been scored, it’s ready to explore interactively with
the Inspect Log Viewer:

``` r
inspect_view(tsk)
```

<img src="man/figures/log_viewer.png" alt="A screenshot of the Inspect Log Viewer, an interactive app displaying information on the 3 samples evaluated in this eval." width="100%" />
