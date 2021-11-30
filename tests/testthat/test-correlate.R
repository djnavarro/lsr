
set.seed(1)
x <- data.frame(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100)
)
y <- data.frame(
  d = rnorm(100),
  e = rnorm(100),
  f = rnorm(100)
)
z <- data.frame(
  g = sample(letters, 100, TRUE),
  h = rnorm(100),
  i = rnorm(100)
)
z$h[1:5] <- NA

test_that("correlation returns s3 object", {

  expect_s3_class(correlate(x), "correlate")
  expect_s3_class(correlate(x, y), "correlate")
  expect_s3_class(correlate(x, z), "correlate")

})

out <- list(
  xx = unclass(correlate(x)),
  xy = unclass(correlate(x, y)),
  xz = unclass(correlate(x, z))
)


test_that("correlation output has expected structure", {

  for(m in 1:length(out)) {

    expect_type(out[[m]], "list")
    expect_length(out[[m]], 5)
    expect_named(out[[m]], c("correlation", "p.value", "sample.size", "args", "tiesProblem"))

    for(i in 1:3) {
      expect_true(inherits(out[[m]][[i]], "matrix"))
    }

    expect_type(out[[m]][[4]], "character")
    expect_type(out[[m]][[5]], "logical")
  }

})

test_that("NA values appear in correct slots", {

  # all correlations with character-valued z$g are NA
  expect_true(all(is.na(out$xz$correlation[,"g"])))

  # sample sizes have been adjusted correctly
  expect_equal(out$xz$sample.size[1, "g"], NA_integer_) # NA for character
  expect_equal(out$xz$sample.size[1, "h"], 95)  # remove NA values
  expect_equal(out$xz$sample.size[1, "i"], 100) # nothing missing

})

