x <- factor(c(1,4,2,2,3,3,5,5,6,6), labels = letters[1:6])
p <- c(5,3,2,1,4,6)

test_that("permuteLevels returns the correct level ordering", {
  expect_equal(levels(permuteLevels(x, perm = p)), c("e","c","b","a","d","f"))
})

test_that("permuteLevels returns a factor", {
  expect_s3_class(permuteLevels(x, perm = p), "factor")
})

test_that("permuteLevels invert = TRUE is the left inverse of the forward permutation", {
  expect_equal(permuteLevels(permuteLevels(x, p), p, invert = TRUE), x)
})

test_that("permuteLevels preserves the data values (only order of levels changes)", {
  result <- permuteLevels(x, perm = p)
  # as.character should give the same labels since only level ordering shifts
  expect_equal(as.character(result), as.character(x))
})

test_that("permuteLevels ordered = TRUE produces an ordered factor", {
  result <- permuteLevels(x, perm = p, ordered = TRUE)
  expect_true(is.ordered(result))
})

test_that("permuteLevels errors when x is not a factor", {
  expect_error(permuteLevels(letters[1:6], p),  '"x" must be a factor')
  expect_error(permuteLevels(1:6, 1:6),          '"x" must be a factor')
})

test_that("permuteLevels errors when perm is not numeric", {
  expect_error(permuteLevels(x, perm = letters[1:6]), '"perm" must be numeric')
})

test_that("permuteLevels errors when perm has wrong length", {
  expect_error(permuteLevels(x, perm = 1:3), 'length of "perm" must equal the number of levels')
})

test_that("permuteLevels errors when perm is not a valid permutation", {
  expect_error(permuteLevels(x, perm = c(1,1,3,4,5,6)), '"perm" is not a valid permutation')
})
