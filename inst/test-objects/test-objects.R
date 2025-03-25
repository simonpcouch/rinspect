# creates objects similar to those from the introductory vignette
# but with three epochs.
withr::local_envvar(list(INSPECT_LOG_DIR = "inst/test-objects/logs/"))

library(ellmer)

are_task_3e <- Task$new(
  dataset = are,
  solver = generate(chat_claude()),
  scorer = model_graded_qa(partial_credit = TRUE),
  name = "An R Eval"
)
are_task_openai_3e <- are_task_3e$clone()

are_task_3e$eval(epochs = 3)
are_task_openai_3e$eval(epochs = 3, solver_chat = chat_openai(model = "gpt-4o"))

save(are_task_3e, file = "inst/test-objects/are_task_3e.rda")
save(are_task_openai_3e, file = "inst/test-objects/are_task_openai_3e.rda")
