# Create all the getters and setters for the configuration
# settings in the package environment
.pkgenv_vars <- names(list_config())

# Template functions to create configuration setters
# TODO: Might not need setters since modify config exists
create_setter_function <- function(parameter) {
  rlang::new_function(
    rlang::exprs(value = ,
                 env = .pkgenv),

    rlang::expr({
      old_config <- rlang::env_get(env, "config")
      # make new config a named list object
      new_parm <- list(value)
      names(new_parm) <- !!parameter
      new_config <- utils::modifyList(old_config, new_parm)
      # Bind the variables to the environment
      rlang::env_bind(env, config = new_config)
      invisible(old_config)
    }),
    rlang::env_parent()
  )
}

# Template function to create configuration getters
create_getter_function <- function(parameter) {
  rlang::new_function(
    rlang::exprs(env = .pkgenv),
    rlang::expr({
      rlang::env_get(env, "config")[[!!parameter]]
    }),
    rlang::env_parent()
  )
}

# Create getters and setters for all configurations options in the
# package environment
setter_funs <- lapply(.pkgenv_vars, create_setter_function)
names(setter_funs) <- paste0("set_",.pkgenv_vars)
getter_funs <- lapply(.pkgenv_vars, create_getter_function)
names(getter_funs) <- paste0("get_", .pkgenv_vars)

# Bind everything to the current evnironment (namespace:eemanalyzeR)
rlang::env_bind(rlang::current_env(), !!!setter_funs)
rlang::env_bind(rlang::current_env(), !!!getter_funs)

