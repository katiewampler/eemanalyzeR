test_that("package defaults load into .pkgenv", {
  # get things in .pkgenv to make sure they load
    cfg <- as.list(.pkgenv)

  # read expected defaults
  builtin <- yaml::read_yaml(
    file.path(system.file("extdata", package = "eemanalyzeR"),
              "eemanalyzeR-config.yml")
  )

  expect_true(is.list(cfg))
  expect_true(length(cfg) > 0)

  # compare names
  expect_setequal(names(cfg), names(builtin))

  # compare default values
  for (n in names(builtin)) {
    expect_equal(cfg[[n]], builtin[[n]], ignore_attr = TRUE)
  }

})
