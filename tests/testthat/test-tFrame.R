Gf <- c(105, 119, 121, 98)
Gc <- c(110, 115, 119, 103)
Gs <- c(112, 102, 108, 99)
dataset <- data.frame(Gf, Gc, Gs)
rownames(dataset) <- paste("person", 1:4, sep = "")

person1 <- c(105, 110, 112)
person2 <- c(119, 115, 102)
person3 <- c(121, 119, 108)
person4 <- c(98,  103, 99)
dataset2 <- data.frame(person1, person2, person3, person4)
rownames(dataset2) <- c("Gf", "Gc", "Gs")

test_that("tFrame produces the expected transposed data frame", {
  # Regression guard for the original test
  expect_equal(tFrame(dataset), dataset2)
})

test_that("tFrame returns a data frame", {
  expect_s3_class(tFrame(dataset), "data.frame")
})

test_that("tFrame swaps the dimensions of the input", {
  result <- tFrame(dataset)
  expect_equal(nrow(result), ncol(dataset))
  expect_equal(ncol(result), nrow(dataset))
})

test_that("tFrame preserves row and column names (transposed)", {
  result <- tFrame(dataset)
  # Original column names become row names of the result
  expect_equal(rownames(result), colnames(dataset))
  # Original row names become column names of the result
  expect_equal(colnames(result), rownames(dataset))
})

test_that("tFrame is its own inverse for square numeric data frames", {
  sq <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  rownames(sq) <- c("r1", "r2", "r3")
  expect_equal(tFrame(tFrame(sq)), sq)
})

test_that("tFrame errors on non-data-frame input", {
  expect_error(tFrame(matrix(1:9, 3, 3)), "intended to apply to data frames only")
  expect_error(tFrame(1:10),              "intended to apply to data frames only")
  expect_error(tFrame(list(a = 1:3)),     "intended to apply to data frames only")
})
