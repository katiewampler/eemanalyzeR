# Create all the getters and setters for the configuration
# settings in the package environment
.pkgenv_vars <- names(.pkgenv)

# Template functions to create configuration setters
create_setter_function <- function(parameter) {
  rlang::new_function(
    rlang::exprs(value = ,
                 env = .pkgenv),

    rlang::expr({
      old <- rlang::env_get(env, !!parameter)
      rlang::env_poke(env, !!parameter, value)
      #old <- .pkgenv[[!!parameter]]
      #.pkgenv[[!!parameter]] <- value
      invisible(old)
    }),
    rlang::env_parent()
  )
}

# Template function to create configuration getters
create_getter_function <- function(parameter) {
  rlang::new_function(
    rlang::exprs(env = .pkgenv),
    rlang::expr({
      rlang::env_get(env, !!parameter)
      #.pkgenv[[!!parameter]]
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
