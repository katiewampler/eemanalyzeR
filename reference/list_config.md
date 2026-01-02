# List current eemanalyzeR configuration

Returns a named list of all configuration options for eemanalyzeR data
processing.

## Usage

``` r
list_config(env = .pkgenv)
```

## Arguments

- env:

  the environment to search for the configuration settings. Defaults to
  the package environment (.pkgenv). It is unlikely the user would
  change this default unless debugging custom processing scripts.

## Value

A named list with the configuration setting names and their values.

## Details

eemanalyzeR allows the user to control data processing via configuration
settings that modify various steps of the automated data processing
code. This function allows the user to see values of the processing
configuration for the current processing environment. Note: This
configuration resets to either the package defaults (stored in package
data as eemanalyzeR-config.yaml) or in the user's default settings
(stored in a separate yaml file on user's computer) when the package is
re-loaded (such as via
[`library(eemanalyzeR)`](https://github.com/katiewampler/eemanalyzeR))

## Examples

``` r
# Get current configuration options
current_settings <- list_config()
```
