save_temp_html <- function(html) {
  file0 <- file.path(tempdir(), "index.html")
  htmltools::save_html(html, file = file0)
  unescape_html(file0)
  file0
}


unescape_html <- function(fpath, escape = F) {
  table0 <- list('&' = '&amp;', '<' = '&lt;', '>' = '&gt;', "'" = '&#39;', '"' = '&quot;')
  readLines(fpath) %>%
    purrr::map_chr(~replace_by_tbl(.x, table0, escape = escape)) %>%
    writeLines(fpath)
}


replace_by_tbl <- function(x, tbl0, escape = F) {
  .f <- function(x, y, z) {
    gsub(pattern = y, replacement = z, x = x)
  }
  param <- if_else(
    escape,
    list(from = names(tbl0), to = tbl0),
    list(from = tbl0, to = names(tbl0))
  )
  purrr::reduce2(param$from, param$to, .f, .init = x)
}


if_else <- function(test, yes, no) {
  if (test) return(yes)
  no
}
