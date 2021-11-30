
outcome <- c(1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4) # data
treatment1 <- factor(c(1,1,1,2,2,2,3,3,3))        # grouping variable
anova1 <- aov(outcome ~ treatment1)               # run the ANOVA

treatment2 <- factor(c( 1,2,3,1,2,3,1,2,3))      # second grouping variable
anova2 <- aov(outcome ~ treatment1 + treatment2) # run the ANOVA


test_that("eta squared example code works", {
  expect_equal(etaSquared(anova1)[1], 0.5497229, tolerance = .0000001)
  expect_equal(etaSquared(anova1)[2], 0.5497229, tolerance = .0000001)

  expect_equal(etaSquared(anova2)[1, 1], 0.5497229, tolerance = .0000001)
  expect_equal(etaSquared(anova2)[2, 1], 0.4305727, tolerance = .0000001)
  expect_equal(etaSquared(anova2)[1, 2], 0.9653961, tolerance = .0000001)
  expect_equal(etaSquared(anova2)[2, 2], 0.9562393, tolerance = .0000001)
})
