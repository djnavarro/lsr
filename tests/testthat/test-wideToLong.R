# Outcome measure is mean response time (MRT), measured in two conditions
# with 4 participants. All participants participate in both conditions.

wide <- data.frame(
  accuracy_t1 = c(.15, .50, .78, .55),
  accuracy_t2 = c(.55, .32, .99, .60),
  id          = 1:4
)

# A more complex design with multiple within-subject factors.
wide2 <- data.frame(
  id            = 1:4,
  gender        = factor(c("male","male","female","female")),
  MRT_cond1_day1 = c(415,500,478,550),
  MRT_cond2_day1 = c(455,532,499,602),
  MRT_cond1_day2 = c(400,490,468,502),
  MRT_cond2_day2 = c(450,518,474,588),
  PC_cond1_day1  = c(79,83,91,75),
  PC_cond2_day1  = c(82,86,90,78),
  PC_cond1_day2  = c(88,92,98,89),
  PC_cond2_day2  = c(93,97,100,95)
)

test_that("wideToLong structural tests pass (regression guard)", {
  w1 <- wideToLong(wide, "time")
  expect_s3_class(w1, "data.frame")
  expect_equal(nrow(w1), 8)
  expect_named(w1, c("id", "time", "accuracy"))

  w2 <- wideToLong(wide2)
  expect_s3_class(w2, "data.frame")
  expect_equal(nrow(w2), 16)
  expect_named(w2, c("id", "gender", "MRT", "PC", "within1", "within2"))

  w3 <- wideToLong(wide2, within = c("condition","day"))
  expect_s3_class(w3, "data.frame")
  expect_equal(nrow(w3), 16)
  expect_named(w3, c("id", "gender", "MRT", "PC", "condition", "day"))

  expect_equal(unname(w2), unname(w3))

  w4 <- wideToLong(wide2, split = FALSE)
  expect_s3_class(w4, "data.frame")
  expect_equal(nrow(w4), 16)
  expect_named(w4, c("id", "gender", "within", "MRT", "PC"))
})

test_that("wideToLong cell values are correct for simple two-timepoint case", {
  w <- wideToLong(wide, "time")
  # participant 1: accuracy_t1 = 0.15 → time=t1; accuracy_t2 = 0.55 → time=t2
  p1 <- w[w$id == 1, ]
  expect_equal(p1$accuracy[p1$time == "t1"], 0.15)
  expect_equal(p1$accuracy[p1$time == "t2"], 0.55)
  # participant 3: accuracy_t1 = 0.78, accuracy_t2 = 0.99
  p3 <- w[w$id == 3, ]
  expect_equal(p3$accuracy[p3$time == "t1"], 0.78)
  expect_equal(p3$accuracy[p3$time == "t2"], 0.99)
})

test_that("wideToLong cell values are correct for MRT in complex design", {
  w <- wideToLong(wide2, within = c("condition","day"))
  # participant 1, condition=cond1, day=day1: MRT should be 415
  row <- w[w$id == 1 & w$condition == "cond1" & w$day == "day1", ]
  expect_equal(row$MRT, 415)
  # participant 2, condition=cond2, day=day2: MRT should be 518
  row2 <- w[w$id == 2 & w$condition == "cond2" & w$day == "day2", ]
  expect_equal(row2$MRT, 518)
})

test_that("wideToLong preserves between-subject variables", {
  w <- wideToLong(wide2, within = c("condition","day"))
  # gender should be preserved per participant
  for (i in 1:4) {
    rows <- w[w$id == i, ]
    expect_true(all(rows$gender == wide2$gender[i]))
  }
})
