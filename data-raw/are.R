# concatenate the json files in `data-raw/are`
are <- 
  list.files("data-raw/are", pattern = "*.json", full.names = TRUE) %>%
  purrr::map(function(.x) {
    .x <- jsonlite::fromJSON(.x)
    .x$knowledge <- list(.x$knowledge)
    as_tibble(.x)
  }) %>%
  purrr::list_rbind() %>%
  dplyr::arrange(title) %>%
  dplyr::rename(id = title)

usethis::use_data(are, overwrite = TRUE)
