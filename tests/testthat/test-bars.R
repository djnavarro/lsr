# super minimal test: bars() needs a lot of rethinking, but at
# a minimum this case should always work

x <- 1:100
y <- factor(sample(c("a", "b"), 100, TRUE))

test_that("bars works in minimal example", {

  expect_silent(bars(x ~ y))
  expect_invisible(bars(x ~ y))
  expect_s3_class(bars(x ~ y), "data.frame")

  # check that it created something
  png_file <- paste0(tempfile(), ".png")
  png(filename = png_file)
  bars(x ~ y)
  dev.off()
  expect_true(file.exists(png_file))
  if(file.exists(png_file)) file.remove(png_file)

})
