# Validate the eemanalyzeR configuration

Checks that all settings in the eemanalyzeR config are valid options and
warns user if not

## Usage

``` r
validate_config(env = .pkgenv)
```

## Arguments

- env:

  environment where to find the config. Defaults to the package
  environment

## Value

invisibly returns TRUE if the configuration is valid, otherwise returns
an error

## Examples

``` r
# Example validation
validate_config()
```
