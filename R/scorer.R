#' Scorer: Evaluate a response using a model
#'
#' @param template Grading template to use--a `glue()` string which will take
#' substitutions `input`, `answer`, `target`, `instructions`.
#' @param instructions Grading instructions.
#' @param grade_pattern A regex pattern to extract the final grade from the
#' judge model's response.
#' @param partial_credit Whether to allow partial credit.
#' @param chat An ellmer chat used to grade the model output.
# in Inspect, "by default, the model being evaluated is used"
#'
#' @returns
#' A function that will grade model responses according to the given instructions.
#'
#' @examples
#' if (interactive()) {
#'   model_graded_qa()
#' }
#'
#' @family scorers
#' @export
model_graded_qa <- function(
  template = NULL,
  instructions = NULL,
  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  chat = ellmer::chat_claude()
) {
  # TODO: type check
  function(input, target, output) {
    model_graded_qa_impl(
      input = input,
      target = target,
      output = output,
      template = template,
      instructions = instructions,
      grade_pattern = grade_pattern,
      partial_credit = partial_credit,
      chat = chat
    )
  }
}

model_graded_qa_impl <- function(
  input,
  target,
  output,
  template = NULL,
  instructions = NULL,
  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  chat = chat_claude()
) {
  template <- template %||% qa_default_template()
  instructions <- instructions %||% qa_default_instructions(partial_credit)

  prompt <- qa_format_prompt(template, input, output, target, instructions)
  chat <- chat$clone()
  response <- chat$chat(prompt)

  result <- qa_extract_grade(response, grade_pattern, partial_credit)

  list(
    result = result,
    chat = chat,
    metadata = list(
      prompt = prompt,
      response = response,
      grade_pattern = grade_pattern
    )
  )
}

qa_format_prompt <- function(template, input, output, target, instructions) {
  glue::glue(
    template,
    input = input,
    answer = output,
    target = target,
    instructions = instructions
  )
}

qa_extract_grade <- function(response, pattern, partial_credit = FALSE) {
  grade_letter <- regmatches(
    response,
    regexec(pattern, response, perl = TRUE)
  )[[1]][2]

  if (is.na(grade_letter)) return(NA)

  switch(
    toupper(grade_letter),
    "C" = 1.0,
    "P" = if (partial_credit) 0.5 else 0.0,
    "I" = 0.0,
    NA
  )
}

qa_default_instructions <- function(partial_credit = FALSE) {
  partial_letter <- if(partial_credit) ", P, or " else " or "
  partial_prompt <- if (partial_credit) '"P" for partially correct answers,' else ""

  glue::glue(
    "After assessing the submitted answer, reply with 'GRADE: $LETTER' where
    LETTER is one of C{partial_letter}I.
    Please choose ONE option: either 'C' for correct answers, {partial_prompt}
    or 'I' for incorrect answers.
    First explain your reasoning, then end with GRADE: $LETTER."
  )
}

# based on https://github.com/UKGovernmentBEIS/inspect_ai/blob/f60a6ff8502fa43ef1b9a7a905eab2a21608f1c1/src/inspect_ai/scorer/_model.py#L206
qa_default_template <- function() {
  "You are assessing a submitted answer on a given task based on a target response.
  [BEGIN DATA]
  ***
  [Task]: {input}
  ***
  [Submission]: {answer}
  ***
  [Target response]: {target}
  ***
  [END DATA]
  Does the submission meet the criterion?
  {instructions}"
}
