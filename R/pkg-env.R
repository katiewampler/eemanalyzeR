# Functions around setting up a processing environment
# Need to make these so it cleans up the processing code and makes passing
# function arguments much more clear.

# TODO create 4 different ways for user to use processing environment variables
# 1) User doesn't do anything then the package defaults are used by run_eems
# 2) User creates a file (stored on their computer) that has processing defaults that
#    eemanalyzer pulls from at load time
# 3) User sets pkg environment defaults in session but that affects any processing
#    that occurs during that session. After package is reloaded the defaults are
#    restored to package defaults
# 4) User supplies arguments to run_eems function that modify processing ONLY during
#    that run.

# TODO Make function for user to get and set defaults for processing and
# save in same place as MDLs on user's computer for consistency

# Create an environment to store EEMS processing arguments and parameters
.pkgenv <- rlang::new_environment(data = list(
  # Optional abs_dir_read arguments with defaults matching abs_dir_read
  abs_pattern = NULL,
  abs_skip = "SEM|BEM|Waterfall",
  abs_file_ext = "dat",
  abs_recurse_read = FALSE,

  # Optional eem_dir_read arguments with defaults matching eem_dir_read
  eem_pattern = NULL,
  eem_skip = "(?i)abs",
  eem_file_ext = "dat",
  eem_recurse_read = FALSE,
  eem_import_func = "aqualog",

  # Optional metadata arguments
  meta_sheet = NULL, # only if excel
  meta_validate = TRUE, # usually we want to validate the metadata
  sample_type_regex = list(iblank_pattern = "BEM$|Waterfall ?Plot ?Blank",
                           sblank_pattern = "Blank|blank|BLK",
                           check_pattern = "Tea|tea"),


  # TODO Optional Processing arguments
  ex_clip = c(247, 450),
  em_clip = c(247, 600),
  type = c(TRUE, TRUE, TRUE, TRUE),
  width = c(16, 3, 30, 10),
  interpolate = c(TRUE, TRUE, FALSE, FALSE),
  method = 1,
  cores = 1,
  cuvle = 1,

  # Saving indices arguments
  index_method = "eemanalyzeR",
  tolerance = 0.2,
  return = "long",
  #cuvle = 1,
  qaqc_dir = NULL,
  arg_names = NULL,

  # Saving the raw files arguments
  filename = "default_filename.csv", # TODO is this needed?
  output_dir = NULL,
  csv = FALSE,
  readme = NULL
),
  parent = rlang::empty_env()
)


# Create all the getters and setters for the package environment
.pkgenv_vars <- names(.pkgenv)

create_setter_function <- function(parameter) {
  rlang::new_function(
    rlang::exprs(value = ),

    rlang::expr({
      old <- .pkgenv[[!!parameter]]
      .pkgenv[[!!parameter]] <- value
      invisible(old)
  }),
rlang::caller_env()
  )
}

create_getter_function <- function(parameter) {
  rlang::new_function(
    NULL,
    rlang::expr({
      .pkgenv[[!!parameter]]
  }),
  rlang::caller_env()
  )
}

# # Try to use lapply to create a bunch of getters and setters from the defaults
setter_funs <- lapply(.pkgenv_vars, create_setter_function)
names(setter_funs) <- paste0("set_",.pkgenv_vars)
getter_funs <- lapply(.pkgenv_vars, create_getter_function)
names(getter_funs) <- paste0("get_", .pkgenv_vars)

rlang::env_bind(rlang::current_env(), !!!setter_funs)
rlang::env_bind(rlang::current_env(), !!!getter_funs)


