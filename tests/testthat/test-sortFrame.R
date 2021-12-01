
dataset <- data.frame(
  txt = c("bob","Clare","clare","bob","eve","eve"),
  num1 = c(3,1,2,0,0,2),
  num2 = c(1,1,3,0,3,2),
  etc = c("not","used","as","a","sort","term"),
  stringsAsFactors = FALSE
)

f1 <- sortFrame(dataset, num1)
f2 <- sortFrame(dataset, num1, num2)
f3 <- sortFrame(dataset, txt)

# test_that("sortFrame example works", {
#   expect_equal(f1$etc, c("a", "sort", "used", "as", "term", "not"))
#   expect_equal(f2$etc, c("a", "sort", "used", "term", "as", "not"))
#   expect_equal(f3$etc, c("not", "a", "as", "used", "sort", "term"))
# })
