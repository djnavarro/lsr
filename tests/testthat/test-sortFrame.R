dataset <- data.frame(
  txt = c("bob", "Clare", "clare", "bob", "eve", "eve"),
  num1 = c(3, 1, 2, 0, 0, 2),
  num2 = c(1, 1, 3, 0, 3, 2),
  etc = c("not", "used", "as", "a", "sort", "term"),
  stringsAsFactors = FALSE
)

test_that("sortFrame sorts correctly by a single numeric column", {
  f1 <- sortFrame(dataset, num1)
  # num1 ascending: 0,0,1,2,2,3 → rows 4,5,2,3,6,1 (stable sort)
  expect_equal(f1$num1, c(0, 0, 1, 2, 2, 3))
  expect_equal(f1$etc, c("a", "sort", "used", "as", "term", "not"))
})

test_that("sortFrame sorts correctly by two numeric columns", {
  f2 <- sortFrame(dataset, num1, num2)
  # primary num1, secondary num2: (0,0),(0,3),(1,1),(2,2),(2,3),(3,1) → rows 4,5,2,6,3,1
  expect_equal(f2$num1, c(0, 0, 1, 2, 2, 3))
  expect_equal(f2$num2, c(0, 3, 1, 2, 3, 1))
  expect_equal(f2$etc, c("a", "sort", "used", "term", "as", "not"))
})

test_that("sortFrame returns data frame with unchanged dimensions", {
  f <- sortFrame(dataset, num1)
  expect_s3_class(f, "data.frame")
  expect_equal(dim(f), dim(dataset))
  expect_named(f, names(dataset))
})

test_that("sortFrame with no sort terms returns the data frame unchanged", {
  f <- sortFrame(dataset)
  expect_equal(f, dataset)
})

test_that("sortFrame alphabetical character sort works for all-lowercase strings", {
  # Use a dataset with no case-mixing to avoid locale-sensitive tie-breaking
  df_lower <- data.frame(
    word = c("cherry", "apple", "banana", "apple", "date"),
    val = 1:5,
    stringsAsFactors = FALSE
  )
  f <- sortFrame(df_lower, word)
  expect_equal(f$word, c("apple", "apple", "banana", "cherry", "date"))
  expect_equal(f$val, c(2L, 4L, 3L, 1L, 5L))
})

test_that("sortFrame alphabetical = FALSE uses ASCII ordering", {
  df <- data.frame(
    txt = c("banana", "Apple", "cherry"),
    val = 1:3,
    stringsAsFactors = FALSE
  )
  # ASCII order: uppercase letters precede lowercase, so "Apple" < "banana" < "cherry"
  f <- sortFrame(df, txt, alphabetical = FALSE)
  expect_equal(f$txt, c("Apple", "banana", "cherry"))
})

test_that("sortFrame reverse-sorts numerics with minus sign", {
  f <- sortFrame(dataset, -num1)
  expect_equal(f$num1, c(3, 2, 2, 1, 0, 0))
})

test_that("sortFrame errors on non-data-frame input", {
  expect_error(sortFrame(1:10, num1), '"x" must be a data frame')
  expect_error(sortFrame(matrix(1:4, 2), num1), '"x" must be a data frame')
  expect_error(sortFrame(list(a = 1), a), '"x" must be a data frame')
})

test_that("sortFrame errors on invalid alphabetical argument", {
  expect_error(sortFrame(dataset, txt, alphabetical = "yes"))
  expect_error(sortFrame(dataset, txt, alphabetical = c(TRUE, FALSE)))
  expect_error(sortFrame(dataset, txt, alphabetical = NA))
})

test_that("sortFrame sorts mixed-case characters correctly (locale-stable)", {
  # Pinning LC_COLLATE to "C" gives ASCII ordering: uppercase letters precede
  # lowercase, so "Clare" sorts before "clare". Without pinning, results vary
  # across platforms (reported in issue #8).
  skip_if_not_installed("withr")
  withr::with_locale(c(LC_COLLATE = "C"), {
    f3 <- sortFrame(dataset, txt)
    # Expected row order: bob(1), bob(4), Clare(2), clare(3), eve(5), eve(6)
    expect_equal(f3$txt, c("bob", "bob", "Clare", "clare", "eve", "eve"))
    expect_equal(f3$etc, c("not", "a", "used", "as", "sort", "term"))
  })
})
