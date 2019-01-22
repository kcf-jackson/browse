# Construct mapping between actual and virtual directories
map_between <- function(v_dirname, actual_paths) {
  file.path2 <- function(x, y) {
    if (x == "") return(y)
    file.path(x, y)
  }
  virtual_paths <- purrr::map2_chr(
    v_dirname, basename(actual_paths), file.path2
  )
  data.frame(
    virtual = virtual_paths, actual = actual_paths,
    stringsAsFactors = F
  )
}


# List virtual directory
list_vfiles <- function(vpath, recursive = F, full.names = F, path_ls) {
  list_fun <- ifelse(recursive, list_exhaustive, list_top_level)
  res <- list_fun(vpath, path_ls)
  postprocess_path(res, vpath, recursive, full.names)
}


# List file with "recursive = T"
list_exhaustive <- function(vpath = ".", path_ls) {
  # Equivalent to list.files(recursive = T)
  if (vpath == "." || vpath == "./") return(path_ls)
  ind <- purrr::map_lgl(path_ls, ~is_descendent(vpath, .x))
  sort(path_ls[ind])
}
# # Check behaviour
# list_exhaustive(".")
# list_exhaustive("tests")
# list_exhaustive("readme.md")


# List file with "recursive = F"
list_top_level <- function(vpath = ".", path_ls) {
  res <- list_exhaustive(vpath, path_ls)
  d <- ifelse(is_parent_dir(vpath), 0, folder_depth(vpath))
  sort(unique(purrr::map_chr(res, ~split_path(.x)[d + 1])))
}
# # Check behaviour
# list_top_level(".")
# list_top_level("tests")
# list_top_level("man")
# list_top_level("inst")
# list_top_level("readme.md")


# Post-process filenames
postprocess_path <- function(path_ls, vpath, recursive, full.names) {
  # Non-recursive case
  if (!recursive) {
    if (!full.names) return(path_ls)
    return(file.path(vpath, path_ls))
  }
  # Recursive case
  if (full.names) {
    if (is_parent_dir(vpath)) return(file.path(".", path_ls))
    return(path_ls)
  }
  crop_name(path_ls, vpath)
}

crop_name <- function(path_ls, vpath) {
  if (is_parent_dir(vpath)) return(path_ls)
  d <- folder_depth(vpath)
  crop <- function(x) {
    cropped_path <- split_path(x)[-seq(d)]
    do.call(file.path, as.list(cropped_path))
  }
  purrr::map_chr(path_ls, crop)
}
