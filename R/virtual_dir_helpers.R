is_parent_dir <- function(x) {
  x == "." || x == "./"
}

split_path <- function(x) {
  unlist(strsplit(x, split = "/"))
}

folder_depth <- function(x) {
  strsplit(x, split = "/") %>% unlist() %>% length()
}


is_descendent <- function(top, bottom) {
  f_vec <- top %>% strsplit(split = "/") %>% unlist()
  sub_vec <- bottom %>% strsplit(split = "/") %>% unlist()
  (length(sub_vec) > length(f_vec)) &&
    all(f_vec == sub_vec[seq_along(f_vec)])
}

# is_child <- function(top, bottom) {
#   f_vec <- top %>% strsplit(split = "/") %>% unlist()
#   sub_vec <- bottom %>% strsplit(split = "/") %>% unlist()
#   (length(sub_vec) - length(f_vec) == 1) &&
#     all(f_vec == sub_vec[seq_along(f_vec)])
# }

