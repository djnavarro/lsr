
test_that("importList example works", {

  lst <- list(a = 1)
  expect_false(exists("a"))
  importList(lst, ask=FALSE)
  expect_true(exists("a"))
  expect_equal(a, 1)
  rm(a)

})
