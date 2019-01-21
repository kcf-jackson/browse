# Take a directory path and generate a JSON-style list of the directory
path_to_list <- function(path) {
  if (!is_dir(path))
    return(get_file_info(path))

  children <- path %>%
    list.files(full.names = T) %>%
    purrr::map(path_to_list)
  append(get_file_info(path), list(children = children))
}

# Get metadata of a file
get_file_info <- function(path) {
  selected <- c('size', 'mtime', 'ctime', 'atime')
  attrs <- c('size', 'modified', 'created', 'accessed')
  append(
    list(label = basename(path), path = path, is_dir = is_dir(path)),
    setNames(as.list(file.info(path)[selected]), attrs)
  )
}

# Check if a path is a directory
is_dir <- function(path) {
  file.info(path)$isdir
}
