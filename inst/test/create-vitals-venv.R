create_vitals_venv <- function() {
  env_name <- "vitals-venv"
  reticulate::virtualenv_create(env_name)
  
  reticulate::virtualenv_install(env_name, packages = c("inspect-ai", "pydantic"))
  
  reticulate::use_virtualenv(env_name)
}
