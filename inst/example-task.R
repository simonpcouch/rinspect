library(ellmer)
  
simple_addition <- tibble::tibble(
  input = c("What's 2+2?", "What's 2+3?"),
  target = c("4", "5")
)

tsk <- task_create(dataset = simple_addition)
tsk <- task_solve(tsk, solver = chat_claude())
tsk <- task_score(tsk, scorer = model_graded_qa())

save(tsk, file = "inst/sandbox/example-task.rda")
