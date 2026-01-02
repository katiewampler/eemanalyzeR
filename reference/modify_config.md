# Modify the eemanalyzeR configuration settings

Allows the user to modify configuration settings that affect data
processing through the remainder of the current processing session (i.e.
until the package is reloaded via \`library(eemanalyzeR)\` or R is
restarted)

## Usage

``` r
modify_config(..., env = .pkgenv)
```

## Arguments

- ...:

  the names of settings the user wishes to modify with their new values.
  NOTE: these MUST be named arguments, otherwise the modification of all
  settings will fail.

- env:

  the environment in which configuration settings is stored. Defaults to
  the package environment. Note: we do not recommend the user modifies
  this argument.

## Value

Invisible returns the named list of the new defaults, but this function
is called mostly for it's side effects.

## Details

There are two ways the user can provide name-value pairs of settings
that they want to modify. The first and most useful way is to provide
them as named arguments directly to this function via the ... variable
arguments. To do this the user would provide the arguments as name =
value separated by commas if multiple settings are to be changed at the
same time

Another option is to provide a named list of name = value pairs. In this
case the named list MUST be prefixed by the \`!!!\` splice operator.

## Examples

``` r
# Modify the cuvette length to two centimeters
modify_config(cuvle = 2)

# Two methods to modify multiple defaults
# 1) via multiple named arugments
modify_config(cuvle = 2, eem_skip = "badeem")

# 2) via a named list
modifications <- list(cuvle = 2, eem_skip = "badeem")
modify_config(!!!modifications)
```
