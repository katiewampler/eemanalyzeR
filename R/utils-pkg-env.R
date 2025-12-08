# Create all the getters and setters for the package environment
.pkgenv_vars <- names(.pkgenv)

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
    rlang::caller_env()
  )
}

create_getter_function <- function(parameter) {
  rlang::new_function(
    rlang::exprs(env = .pkgenv),
    rlang::expr({
      rlang::env_get(env, !!parameter)
      #.pkgenv[[!!parameter]]
    }),
    rlang::caller_env()
  )
}

# Create a bunch of getters and setters from the defaults
setter_funs <- lapply(.pkgenv_vars, create_setter_function)
names(setter_funs) <- paste0("set_",.pkgenv_vars)
getter_funs <- lapply(.pkgenv_vars, create_getter_function)
names(getter_funs) <- paste0("get_", .pkgenv_vars)

rlang::env_bind(rlang::current_env(), !!!setter_funs)
rlang::env_bind(rlang::current_env(), !!!getter_funs)
