# # Find a file in a directory
# # @return The path to the file
# locate <- function(path, dir = "./") {
#   all_files <- list.files(dir, recursive = T)
#   all_files[which(path == basename(all_files))]
# }
#
# test_equal(
#   locate_in("locate_file.R"),
#   "R/locate_file.R"
# )


# Open file via terminal
open_file <- function(msg) {
  # .Platform$OS.type: linux -> xdg-open, mac -> open, windows -> none
  if (.Platform$OS.type == "windows") {
    # Windows
    return(system(glue::glue("rstudio {msg$path}")))
  } else {
    # Mac OSX
    osx_check_1 <- grepl("^darwin", R.version$os)
    osx_check_2 <- tolower(Sys.info()["sysname"]) == "darwin"
    if (osx_check_1 || osx_check_2) {
      return(system(glue::glue("open -a rstudio {msg$path}")))
    }
    # Linux
    return(system(glue::glue("rstudio {msg$path}")))
  }
}
