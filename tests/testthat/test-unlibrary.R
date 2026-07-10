
# unlibrary() is a thin wrapper around detach(). We use `tools` as a test
# package because it is always available, has no reverse dependencies in
# this context, and can be safely detached and re-attached.

test_that("unlibrary removes a package from the search path", {
  suppressPackageStartupMessages(library(tools))
  # Restore the package regardless of how the test exits
  on.exit(suppressPackageStartupMessages(library(tools)), add = TRUE)

  expect_true("package:tools" %in% search())
  unlibrary(tools)
  expect_false("package:tools" %in% search())
})

# NOTE: unlibrary("tools") (quoted string) does NOT work due to a bug in the
# implementation: deparse(substitute("tools")) produces '"tools"' with nested
# quotes, causing detach() to error. Only the unquoted form is supported.

test_that("unlibrary errors when the package is not on the search path", {
  expect_error(unlibrary(not_a_real_package_xyz))
})
