#' Create an app from a html object of class "shiny.tag"
#' @param html A html object of class "shiny.tag".
#' @param user_function A R function that takes and returns a named list.
build_app <- function(html, user_function = identity) {
  pipe_fun <- add_pipe(user_function)
  html_in_chr <- html %>%
    save_temp_html() %>%    # save to file so that head tag shows up
    readLines() %>%
    paste(collapse = "\n")
  app <- list(
    call = function(req) {
      list(
        status = 200L,
        headers = list("Content-Type" = "text/html"),
        body = html_in_chr
      )
    },
    onWSOpen = function(ws) {
      ws$onMessage(function(binary, input) {
        output <- pipe_fun(input)
        ws$send(output)
      })
    }
  )
  app
}

# Add 'pipes' to convert from and to JSON
add_pipe <- function(user_fun) {
  purrr::compose(jsonlite::toJSON, user_fun, jsonlite::fromJSON)
}


# Run file browser
run_app <- function(app, port = 9454, display = F) {
  if (display) {
    address <- paste0("http://localhost:", port)
    browseURL(address, browser = getOption("viewer"))
  }
  s <- httpuv::runServer("0.0.0.0", port, app, 250)
  on.exit(s$stop())
}

# Run daemon file browser
run_daemon <- function(app, port = 9454, reminder = T) {
  is_windows <- function () {
    tolower(.Platform$OS.type) == "windows"
  }

  R_binary <- function () {
    R_exe <- ifelse (is_windows(), "R.exe", "R")
    return(file.path(R.home("bin"), R_exe))
  }

  if (reminder) {
    cat(glue::glue("
      ============================================================
      A friendly reminder: You should assign the output of this
      function call to a variable since you need that for closing
      the app later.
      ** USAGE **: SOME_VAR <- browse::directory(A_PATH)
      "
    ))
    run <- readline("Continue? [y/n]: ")
    if (run != 'y') return(invisible(NULL))
  }

  cat("Starting file browser...")
  my_app <- list(app = app, port = port)
  app_file <- tempfile()
  save(my_app, file = app_file)

  load_cmd <- sprintf("load('%s', verbose = T)\n", app_file)
  run_server_cmd <- "browse:::run_app(my_app$app, my_app$port)\n"
  handle <- subprocess::spawn_process(R_binary(), '--slave')  # launch background R process
  subprocess::process_read(handle, timeout = 1000)
  subprocess::process_write(handle, load_cmd)        # load object
  subprocess::process_read(handle, timeout = 1000)
  subprocess::process_write(handle, run_server_cmd)  # run server
  subprocess::process_read(handle, timeout = 1000)

  address <- paste0("http://localhost:", port)
  browseURL(address, browser = getOption("viewer"))
  handle
}


#' Stop app
#' @param handle A subprocess handle; output from `browse::directory`.
#' @export
stop_app <- function(handle) {
  subprocess::process_kill(handle)
  handle
}
