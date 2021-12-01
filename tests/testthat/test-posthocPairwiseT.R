dataset <- data.frame(
  outcome = c( 1,2,3, 2,3,4, 5,6,7 ),
  group = factor(c( "a","a","a", "b","b","b","c","c","c"))
)
anova1 <- aov(outcome ~ group, data = dataset)

test_that("posthocPairwiseT example works", {

  expect_equal(
    posthocPairwiseT(anova1),
    with(dataset, pairwise.t.test(outcome, group))
  )
})


