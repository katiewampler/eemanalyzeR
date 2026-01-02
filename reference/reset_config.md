# Reset all eemanalyzeR settings to package defaults

Returns the data processing configuration settings for this R session
back to package defaults.

## Usage

``` r
reset_config(env = .pkgenv)
```

## Arguments

- env:

  the environment that stores the processing settings. Defaults to the
  package environment. It is not recommended the user modifies this
  argument.

## Value

Invisibly returns the reset default configuration settings as a named
list.

## Details

This allows the user to return the data processing settings back to the
default configuration of the eemanalyzeR package. These defaults are
documented in data.R under "default_config". This function is provided
in case the user needs to return back to default processing settings
after experimenting with modifying the settings using \`modify_config\`.

## Examples

``` r
# Reset the configuration back to package defaults
reset_config()
```
