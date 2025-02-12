last_assistant_turn <- function(turns) {
  for (i in rev(seq_along(turns))) {
    if (turns[[i]]@role == "assistant") {
      return(turns[[i]])
    }
  }
  return(NULL)
}
