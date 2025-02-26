# concatenate the json files in `data-raw/tre`
tre <- 
  list.files("data-raw/tre", pattern = "*.json", full.names = TRUE) %>%
  purrr::map(function(.x) {
    .x <- jsonlite::fromJSON(.x)
    .x$knowledge <- as.list(.x$knowledge)
    as_tibble(.x)
  }) %>%
  purrr::list_rbind() %>%
  dplyr::arrange(title)

usethis::use_data(tre, overwrite = TRUE)
