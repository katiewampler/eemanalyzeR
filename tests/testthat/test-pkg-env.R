# BEFORE ANYTHING RESET THE EEMANALYZER TO DEFAULTS SO IT DOESN'T DEPEND ON ANY OTHER TESTS
reset_eemanalyzer_settings()

test_that("pkg environment has correct defaults", {
  # list of wanted defaults (have to make sure they are in the same order)
  package_default_list <- eemanalyzer_processing_defaults[order(names(eemanalyzer_processing_defaults))]
  package_defaults_from_env <- list_eemanalyzer_settings()[order(names(list_eemanalyzer_settings()))]
  expect_identical(package_default_list, package_defaults_from_env)
})

test_that("pkg environment can be modified by modify_defaults and then reset back to defaults", {
  new_default_list <- list(
    # Text
    abs_pattern = "testpass", 
    # Vector of numbers
    em_clip = c(300, 400),
    # Logical
    csv = FALSE,
    # List
    sample_type_regex = list(
      iblank_pattern = "test_iblank",
      sblank_pattern = "test_sblank",
      check_pattern = "test_tea"
    )
  )
  # The function to modify the defaults
  modify_eemanalyzer_settings(.pkgenv, !!!new_default_list)

  expect_identical(list(
    abs_pattern = get_abs_pattern(),
    em_clip = get_em_clip(),
    csv = get_csv(),
    sample_type_regex = get_sample_type_regex()
  ),
  new_default_list)

  # Check the package resets back to defaults
  package_default_list <- eemanalyzer_processing_defaults[order(names(eemanalyzer_processing_defaults))]

  reset_eemanalyzer_settings()
  package_defaults_from_env <- list_eemanalyzer_settings()[order(names(list_eemanalyzer_settings()))]
  expect_identical(package_default_list, package_defaults_from_env)
  }
)

test_that("modify_defaults can work inside function without modifying package environment", {

  .fnenv <- rlang::env_clone(.pkgenv)
  new_default_list <- list(
    # Text
    abs_pattern = "testpass", 
    # Vector of numbers
    em_clip = c(300, 400),
    # Logical
    csv = FALSE,
    # List
    sample_type_regex = list(
      iblank_pattern = "test_iblank",
      sblank_pattern = "test_sblank",
      check_pattern = "test_tea"
    )
  )
  # The functio to modify the defaults
  modify_eemanalyzer_settings(.fnenv, !!!new_default_list)

  expect_identical(list(
    abs_pattern = get_abs_pattern(.fnenv),
    em_clip = get_em_clip(.fnenv),
    csv = get_csv(.fnenv),
    sample_type_regex = get_sample_type_regex(.fnenv)
  ),
  new_default_list)

  package_default_list <- list(
    abs_pattern = get_abs_pattern(),
    em_clip = get_em_clip(),
    csv = get_csv(),
    sample_type_regex = get_sample_type_regex()
  )

  expect_false(identical(package_default_list, new_default_list))

})
