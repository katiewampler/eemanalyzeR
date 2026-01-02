# Commenting this out but hanging onto it in case we want to add .onLoad functionality later
.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
}