
gender <- factor(c( "male","male","male","male","female","female",
                    "female","male","male","male"))

test_that("goodnessOfFitTest example works", {

  expect_s3_class(goodnessOfFitTest(gender), "gofTest")
  expect_warning(goodnessOfFitTest(gender, p=c(.4,.6)), "chi-squared approximation")

  expect_equal(
    goodnessOfFitTest(gender)$p.value,
    chisq.test(table(gender))$p.value
  )

})
