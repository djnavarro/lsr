long <- data.frame(
  id = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  time = c("t1", "t1", "t1", "t2", "t2", "t2", "t3", "t3", "t3"),
  accuracy = c(.50, .03, .72, .94, .63, .49, .78, .71, .16)
)


test_that("longToWide example works", {
  expect_s3_class(longToWide(long, accuracy ~ time), "data.frame")
  expect_equal(dim(longToWide(long, accuracy ~ time)), c(3, 4))
  expect_named(longToWide(long, accuracy ~ time),
               c("id", "accuracy_t1", "accuracy_t2", "accuracy_t3"))
})
