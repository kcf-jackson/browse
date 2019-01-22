context("Test virtual directory helper functions")

test_that("Test folder_depth", {
  expect_equal(folder_depth("A"), 1)
  expect_equal(folder_depth("A/B"), 2)
  expect_equal(folder_depth("A/B/C"), 3)
  expect_equal(folder_depth("A/B/C/"), 3)
})

test_that("Test is_descendent", {
  expect_equal(is_descendent("A", "A/B"), T)
  expect_equal(is_descendent("A", "A/B/"), T)
  expect_equal(is_descendent("A", "A2/B/"), F)
  expect_equal(is_descendent("A/B", "A"), F)
  expect_equal(is_descendent("A/B", "A/B/C"), T)
  expect_equal(is_descendent("A/B", "A/B/C/D"), T)
  expect_equal(is_descendent("A/B/C", "A/B"), F)
})

# test_that("Test is_child", {
#   expect_equal(is_child("A", "A/B"), T)
#   expect_equal(is_child("A", "A/B/"), T)
#   expect_equal(is_child("A", "A2/B/"), F)
#   expect_equal(is_child("A/B", "A"), F)
#   expect_equal(is_child("A/B", "A/B/C"), T)
#   expect_equal(is_child("A/B", "A/B/C/D"), F)
#   expect_equal(is_child("A/B/C", "A/B"), F)
# })
