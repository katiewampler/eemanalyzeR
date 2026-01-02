# BEFORE ANYTHING RESET THE EEMANALYZER TO DEFAULTS SO IT DOESN'T DEPEND ON ANY OTHER TESTS

test_that("pkg environment gets created", {
  reset_config()
  expect_true(rlang::is_environment(.pkgenv))
})

test_that("pkg environment has correct defaults", {
  reset_config()

  # list of wanted defaults (have to make sure they are in the same order)
  package_default_list <- default_config[order(names(default_config))]
  package_defaults_from_env <- list_config()[order(names(list_config()))]
  expect_identical(package_default_list, package_defaults_from_env)
})

test_that("pkg environment can be modified by modify_defaults and then reset back to defaults", {
  reset_config()

  new_default_list <- list(
    # Text
    abs_pattern = "testpass",
    # Vector of numbers
    em_clip = c(300, 400),
    # Logical
    csv = FALSE,
    # Text
    iblank_pattern = "test_iblank",
    sblank_pattern = "test_sblank",
    check_pattern = "test_tea"
  
  )

  expect_true(rlang::is_environment(.pkgenv))

  # The function to modify the defaults
  modify_config(!!!new_default_list, env = .pkgenv)

  expect_true(rlang::is_environment(.pkgenv))

  expect_identical(list(
    abs_pattern = get_abs_pattern(),
    em_clip = get_em_clip(),
    csv = get_csv(),
    iblank_pattern = get_iblank_pattern(),
    sblank_pattern = get_sblank_pattern(),
    check_pattern = get_check_pattern()
  ),
  new_default_list)

  # Check the package resets back to defaults
  package_default_list <- default_config[order(names(default_config))]

  reset_config()
  package_defaults_from_env <- list_config()[order(names(list_config()))]
  expect_identical(package_default_list, package_defaults_from_env)
  }
)

test_that("modify_defaults can work inside function without modifying package environment", {
  reset_config()

  .fnenv <- rlang::env_clone(.pkgenv)
  new_default_list <- list(
    # Text
    abs_pattern = "testpass",
    # Vector of numbers
    em_clip = c(300, 400),
    # Logical
    csv = FALSE,
    # Text
    iblank_pattern = "test_iblank",
    sblank_pattern = "test_sblank",
    check_pattern = "test_tea"
  )
  # The functio to modify the defaults
  modify_config(!!!new_default_list, env = .fnenv)

  expect_identical(list(
    abs_pattern = get_abs_pattern(.fnenv),
    em_clip = get_em_clip(.fnenv),
    csv = get_csv(.fnenv),
    iblank_pattern = get_iblank_pattern(.fnenv),
    sblank_pattern = get_sblank_pattern(.fnenv),
    check_pattern = get_check_pattern(.fnenv)
  ),
  new_default_list)

  package_default_list <- list(
    abs_pattern = get_abs_pattern(),
    em_clip = get_em_clip(),
    csv = get_csv(),
    iblank_pattern = get_iblank_pattern(),
    sblank_pattern = get_sblank_pattern(),
    check_pattern = get_check_pattern()
  )

  expect_false(identical(package_default_list, new_default_list))

})

