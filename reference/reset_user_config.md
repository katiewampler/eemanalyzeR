# Reset all eemanalyzeR settings in the user configuration file to package defaults

This allows the user to overwrite the data processing settings in the
user configuration file back to the default configuration of the
eemanalyzeR package. These defaults are documented in data.R under
"default_config". This function is provided in case the user has a
malformed configuration file or wants to revert back to default
processing settings after experimenting with modifying the settings
using \`edit_user_config\`.

## Usage

``` r
reset_user_config()
```

## Value

Invisibly returns the reset default configuration settings as a named
list.

## Examples

``` r
reset_user_config()
#> Error in reset_user_config(): No User Config found. Please create using edit_user_config.
load_user_config()
#> User configuration not found. If eemanalyzeR package defaults are not ok, create user configuration with edit_config()
```
