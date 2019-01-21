# # Find a file in a directory
# # @return The path to the file
# locate <- function(path, dir = "./") {
#   all_files <- list.files(dir, recursive = T)
#   all_files[which(path == basename(all_files))]
# }


# Open file via terminal
open_file <- function(msg) {
  # .Platform$OS.type
  # linux -> xdg-open, mac -> open
  system(glue::glue("open -a rstudio {msg$path}"))
}