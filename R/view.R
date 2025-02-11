#' Launch the Inspect Log Viewer
#'
#' @param log_dir Path to directory containing eval logs.
#' Defaults to package example logs.
#' @param host Host to serve on. Defaults to "127.0.0.1",
#' @param port Port to serve on. Defaults to 7576, one greater than the Python
#' implementation.
#' @export
inspect_view <- function(
    log_dir = system.file("logs", package = "rinspect"),
    host = "127.0.0.1",
    port = 7576
) {
  dist_dir <- system.file("dist", package = "rinspect")

  tryCatch({
    existing_server <- httpuv::listServers()
    if (length(existing_server) > 0) {
      httpuv::stopServer(existing_server[[1]])
    }
  }, error = function(cnd) {
    cli::cli_abort("Unable to terminate the existing server.", parent = cnd)
  })

  if (!dir.exists(log_dir)) {
    cli::cli_abort("Log directory {.file {log_dir}} not found.")
  }

  server <- httpuv::startServer(
    host = host,
    port = port,

    app = list(
      call = function(req) {
        tryCatch({
          # parse query parameters
          query <- parseQueryString(req$QUERY_STRING)

          # handle API routes first
          if (startsWith(req$PATH_INFO, "/api/")) {

            # GET /api/logs
            if (req$PATH_INFO == "/api/logs") {
              files <- list.files(log_dir, pattern = "\\.json$", recursive = TRUE)
              log_files <- lapply(files, function(f) {
                file_path <- file.path(log_dir, f)
                info <- file.info(file_path)

                list(
                  name = f,
                  size = info$size,
                  mtime = as.numeric(info$mtime) * 1000
                )
              })

              resp <- list(
                log_dir = normalizePath(log_dir),
                files = log_files
              )

              return(list(
                status = 200,
                headers = list(
                  'Content-Type' = 'application/json',
                  'Cache-Control' = 'no-cache'
                ),
                body = jsonlite::toJSON(resp, auto_unbox = TRUE, null = "null")
              ))
            }

            # GET /api/log-headers
            log_headers <- list(
              status = 200,
              headers = list(
                'Content-Type' = 'application/json',
                'Cache-Control' = 'no-cache'
              )
            )
            if (req$PATH_INFO == "/api/log-headers") {
              if (!is.null(query$file)) {
                files <- if (is.character(query$file) && length(query$file) == 1) {
                  list(query$file)
                } else {
                  as.list(query$file)
                }
                
                headers <- lapply(files, function(f) {
                  file_path <- file.path(log_dir, f)
                  if (file.exists(file_path)) {
                    content <- jsonlite::fromJSON(file_path)
                    # Extract just the metadata/header portion
                    list(
                      task = content$task,
                      task_id = content$task_id,
                      created = content$created,
                      model = content$model
                    )
                  } else {
                    NULL
                  }
                })

                log_headers$body <- 
                  jsonlite::toJSON(headers, auto_unbox = TRUE, null = "null")
                return(log_headers)
              }

              log_headers$body <- 
                jsonlite::toJSON(list(), auto_unbox = TRUE, null = "null")
              return(log_headers)
            }

            # GET /api/logs/{filename}
            if (startsWith(req$PATH_INFO, "/api/logs/")) {
              file <- substr(req$PATH_INFO, 11, nchar(req$PATH_INFO))
              file <- utils::URLdecode(file)
              file_path <- file.path(log_dir, file)

              if (file.exists(file_path)) {
                # read the file content first
                content <- jsonlite::fromJSON(file_path)

                # check header_only parameter
                header_only <- query$`header-only`
                if (!is.null(header_only)) {
                  header_only <- as.numeric(header_only)
                  file_size_mb <- file.info(file_path)$size / (1024 * 1024)

                  if (!is.na(header_only) && file_size_mb > header_only) {
                    # include only metadata and first few records
                    if (!is.null(content$records) && length(content$records) > 10) {
                      content$records <- head(content$records, 10)
                    }
                  }
                }

                return(list(
                  status = 200,
                  headers = list(
                    'Content-Type' = 'application/json',
                    'Cache-Control' = 'no-cache'
                  ),
                  body = jsonlite::toJSON(content, auto_unbox = TRUE, null = "null")
                ))
              }

              return(list(status = 404, body = "File not found"))
            }

            return(list(status = 404, body = "API endpoint not found"))
          }

          # then handle static files
          if (req$PATH_INFO == "/" || !grepl("\\.", req$PATH_INFO)) {
            # serve index.html for root or routes without file extensions
            index_path <- file.path(dist_dir, "index.html")
            return(list(
              status = 200,
              headers = list(
                'Content-Type' = 'text/html',
                'Cache-Control' = 'no-cache'
              ),
              body = readBin(index_path, "raw", file.info(index_path)$size)
            ))
          } else {
            # serve static files from dist
            file_path <- file.path(dist_dir, substring(req$PATH_INFO, 2))
            if (file.exists(file_path)) {
              content_type <- switch(tools::file_ext(file_path),
                "html" = "text/html",
                "js" = "application/javascript",
                "css" = "text/css",
                "svg" = "image/svg+xml",
                "application/octet-stream"
              )
              return(list(
                status = 200,
                headers = list(
                  'Content-Type' = content_type,
                  'Cache-Control' = 'no-cache'
                ),
                body = readBin(file_path, "raw", file.info(file_path)$size)
              ))
            }
          }

          list(status = 404, body = "Not found")

        }, error = function(e) {
          # log the error for debugging
          message("Error processing request: ", e$message)
          list(
            status = 500,
            headers = list('Content-Type' = 'text/plain'),
            body = paste("Error:", e$message)
          )
        })
      }
    )
  )

  url <- sprintf("http://%s:%d", host, port)
  cli::cli_inform(
    c("v" = "Inspect Viewer running at: {.url {url}}"),
    class = "rinspect_viewer_start"
  )

  if (interactive() && !is_testing()) {
    utils::browseURL(url)
  }

  invisible(server)
}

# Helper function to parse query string
parseQueryString <- function(query_string) {
  if (is.null(query_string) || query_string == "") {
    return(list())
  }

  parts <- strsplit(query_string, "&")[[1]]
  params <- lapply(parts, function(p) {
    kv <- strsplit(p, "=")[[1]]
    if (length(kv) == 2) {
      val <- utils::URLdecode(kv[2])
      res <- list(val)
      names(res) <- kv[1]
      return(res)
    }
    NULL
  })

  do.call(c, params)
}
