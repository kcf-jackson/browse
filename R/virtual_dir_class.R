#' R6 virtual directory object class
#' @export
virtual_dir <- R6::R6Class(
  "virtual_directory",
  public = list(
    virtual_dir = NULL,

    lookup_table = NULL,

    initialize = function(v_dirname, actual_paths) {
      table <- map_between(v_dirname, actual_paths)
      self$virtual_dir <- table$virtual
      self$lookup_table <- table
    },

    print = function() {
      print(self$lookup_table)
      invisible(self)
    },

    list_vfiles = function(vpath, recursive = F, full.names = F) {
      list_vfiles(vpath, recursive, full.names, self$virtual_dir)
    },

    # Take a directory path and generate a JSON-style list of the directory
    vpath_to_list = function(vpath = ".") {
      if (!self$is_vdir(vpath)) {
        path <- lookup(vpath, self$lookup_table)
        return(get_file_info(path))
      }
      children <- vpath %>%
        self$list_vfiles(full.names = T) %>%
        purrr::map(~self$vpath_to_list(remove_dot_slash(.x)))
      append(
        list(label = basename(vpath), path = vpath, is_dir = TRUE),
        list(children = children)
      )
    },

    is_vdir = function(vpath) {
      !(basename(vpath) %in% purrr::map_chr(self$virtual_dir, basename))
    },

    directory = function(vpath = ".", skip_first_level = F, reminder = T) {
      skip_first <- ifelse(
        skip_first_level, function(x) { x$children }, identity
      )
      vpath %>%
        self$vpath_to_list() %>%
        skip_first() %>%
        jsonlite::toJSON(auto_unbox = T) %>%
        fix_bracket() %>%
        JSON_to_app(reminder = reminder)
    }
  )
)

remove_dot_slash = function(x) {
  if (stringr::str_sub(x, 1, 2) == "./")
    return(stringr::str_sub(x, 3))
  x
}

lookup = function(x, table) {
  # table is a dataframe with column (chr) "virtual" and "actual"
  ind <- which(table$virtual == x)
  table$actual[ind]
}
