x <- factor( c(1,4,2,2,3,3,5,5,6,6), labels=letters[1:6])
p <- c(5,3,2,1,4,6)

test_that("permuteLevels examples work", {

  expect_s3_class(
    permuteLevels(x, perm = p),
    "factor"
  )

  expect_equal(levels(permuteLevels(x, perm = p)), c("e", "c", "b", "a", "d", "f"))
  expect_equal(permuteLevels(permuteLevels(x, p), p, invert = TRUE), x)

})
