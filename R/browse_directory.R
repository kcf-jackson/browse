#' Browse directory
#' @param path character; the path to the directory
#' @param skip_first_level T or F; should the first level be skipped?
#' @param reminder T or F; should USAGE be displayed?
#' @export
directory <- function(path, skip_first_level = F, reminder = T) {
  path %>%
    dir_to_JSON(skip_first_level = skip_first_level) %>%
    JSON_to_app(reminder = reminder)
}


#' Directory to JSON
#' @param path character; the path to the directory
#' @param skip_first_level T or F; should the first level be skipped?
dir_to_JSON <- function(path, skip_first_level = F) {
  skip_first <- ifelse(
    skip_first_level, function(x) { x$children }, identity
  )
  path %>%
    normalizePath() %>%
    path_to_list() %>%
    skip_first() %>%
    jsonlite::toJSON(auto_unbox = T) %>%
    fix_bracket()
}
fix_bracket <- function(x) {
  start_curly <- (stringr::str_sub(x, 1, 1) == "{")
  end_curly <- (stringr::str_sub(x, -1, -1) == "}")
  if (start_curly && end_curly)
    return(paste("[", x, "]", sep = ""))
  x
}


#' Launch app from JSON
#' @param json character; the JSON string containing the data.
#' @param reminder T or F; should USAGE be displayed?
JSON_to_app <- function(json, reminder = T) {
  json %>%
    build_html() %>%           # html ready
    build_app(open_file) %>%   # app ready
    # run_app(display = T)     # for testing
    run_daemon(reminder = reminder)   # launch app
}
