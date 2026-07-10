test_that("importList example works", {
  lst <- list(a = 1)
  expect_false(exists("a"))
  importList(lst, ask = FALSE)
  expect_true(exists("a"))
  expect_equal(a, 1)
  rm(a)
})

test_that("importList returns 1 invisibly after importing", {
  lst <- list(b = 42)
  ret <- importList(lst, ask = FALSE)
  expect_equal(ret, 1L)
  rm(b)
})

test_that("importList imports multiple elements", {
  lst <- list(x = 10, y = 20, z = 30)
  importList(lst, ask = FALSE)
  expect_true(exists("x") && exists("y") && exists("z"))
  expect_equal(x, 10); expect_equal(y, 20); expect_equal(z, 30)
  rm(x, y, z)
})

test_that("importList coerces invalid names with make.names", {
  lst <- list("group A" = 1:3, "group B" = 4:6)
  importList(lst, ask = FALSE)
  expect_true(exists("group.A"))
  expect_true(exists("group.B"))
  rm(group.A, group.B)
})

test_that("importList errors on non-list input", {
  expect_error(importList(1:5,          ask = FALSE), '"x" must be a list or data frame')
  expect_error(importList("not a list", ask = FALSE), '"x" must be a list or data frame')
  expect_error(importList(42L,          ask = FALSE), '"x" must be a list or data frame')
})

test_that("importList errors on invalid ask argument", {
  # NA is logical in R so it passes the is(ask, "logical") guard, but
  # causes an error downstream when if(NA) is evaluated; still an error.
  expect_error(importList(list(a = 1), ask = NA))
  expect_error(importList(list(a = 1), ask = c(TRUE, FALSE)), '"ask" must be a single logical value')
  expect_error(importList(list(a = 1), ask = "yes"),           '"ask" must be a single logical value')
})
