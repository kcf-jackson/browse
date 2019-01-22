context("Test virtual directory")

test_that("Test list_vfiles_exhaustive against list.files", {
  path_ls <- list.files("./", recursive = T)
  for (path in c(".", "man", "inst", "LICENSE")) {
    expect_equal(
      list_vfiles(path, recursive = T, path_ls = path_ls),
      list.files(path, recursive = T)
    )
    expect_equal(
      list_vfiles(path, recursive = T, full.names = T, path_ls = path_ls),
      list.files(path, recursive = T, full.names = T)
    )
    expect_equal(
      list_vfiles(path, full.names = T, path_ls = path_ls),
      list.files(path, full.names = T)
    )
    expect_equal(
      list_vfiles(path, path_ls = path_ls),
      list.files(path)
    )
  }
})
