# concatenate the json files in `data-raw/are`

paths <- list.files("data-raw/are", pattern = "*.yaml", full.names = TRUE)
raw <- purrr::map(paths, function(.x) {
  .x <- yaml::read_yaml(.x)
  .x$knowledge <- list(.x$knowledge)
  .x
})

jsonlite::write_json(raw, "data-raw/are.json", pretty = TRUE, auto_unbox = TRUE)

are <- raw |>
  purrr::map(\(x) tibble::as_tibble(x)) |>
  purrr::list_rbind() |>
  dplyr::arrange(title) |>
  dplyr::rename(id = title)

usethis::use_data(are, overwrite = TRUE)
