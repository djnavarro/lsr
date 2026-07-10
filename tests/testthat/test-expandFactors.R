grading <- data.frame(
  teacher = factor(c("Amy","Amy","Ben","Ben","Cat")),
  gender  = factor(c("male","female","female","male","male")),
  grade   = c(75, 80, 45, 50, 65)
)

test_that("expandFactors returns a data frame with the right dimensions", {
  result <- expandFactors(grading)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(grading))
  # 2 factor levels for teacher (Ben,Cat) + 1 for gender (male) + grade = 4 cols
  expect_equal(ncol(result), 4L)
})

test_that("expandFactors produces correct treatment contrast values", {
  result <- expandFactors(grading)
  # teacherBen should be 1 for Amy=Ben rows, 0 elsewhere
  expect_equal(result$teacherBen, as.numeric(grading$teacher == "Ben"))
  # teacherCat should be 1 for Cat rows only
  expect_equal(result$teacherCat, as.numeric(grading$teacher == "Cat"))
  # gendermale: 1 where gender == "male"
  expect_equal(result$gendermale, as.numeric(grading$gender == "male"))
  # grade is unchanged
  expect_equal(result$grade, grading$grade)
})

test_that("expandFactors with a data frame that has no factors returns numeric columns only", {
  df_numeric <- data.frame(x = 1:4, y = c(2.1, 3.2, 1.5, 4.0))
  result <- expandFactors(df_numeric)
  expect_s3_class(result, "data.frame")
  # No factors to expand, so just x and y remain
  expect_equal(ncol(result), 2L)
  expect_equal(result$x, df_numeric$x)
  expect_equal(result$y, df_numeric$y)
})

test_that("expandFactors errors when data is not a data frame", {
  expect_error(expandFactors(list(a = 1:3, b = factor(c("x","y","x")))),
               '"data" must be a data frame')
  expect_error(expandFactors(matrix(1:4, 2, 2)),
               '"data" must be a data frame')
})

test_that("expandFactors handles a factor with unused levels", {
  df <- data.frame(
    grp   = factor(c("a","a","b"), levels = c("a","b","c")),
    score = 1:3
  )
  result <- expandFactors(df)
  # Level "c" has no observations but should still generate a column
  expect_true("grpb" %in% names(result) | "grpc" %in% names(result))
})
