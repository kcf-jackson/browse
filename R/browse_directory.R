#' Browse directory
#' @param path character; the path to the directory
#' @param reminder T or F; should USAGE be displayed?
#' @export
directory <- function(path, reminder = T) {
  path %>%
    normalizePath() %>%
    path_to_list() %>%
    jsonlite::toJSON() %>%     # data ready
    build_html() %>%           # html ready
    build_app(open_file) %>%   # app ready
    # run_app(display = T)     # for testing
    run_daemon(reminder = reminder)   # launch app
}
