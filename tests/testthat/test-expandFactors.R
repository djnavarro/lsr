grading <- data.frame(
  teacher = factor(c("Amy", "Amy", "Ben", "Ben", "Cat")),
  gender = factor(c("male", "female", "female", "male", "male")),
  grade = c(75, 80, 45, 50, 65)
)

test_that("expandFactors example works", {

  expect_s3_class(expandFactors(grading), "data.frame")
  expect_equal(nrow(expandFactors(grading)), nrow(grading))
  expect_equal(ncol(expandFactors(grading)), 4)

  expect_equal(
    expandFactors(grading)$teacherBen,
    as.numeric(grading$teacher == "Ben")
  )

})
