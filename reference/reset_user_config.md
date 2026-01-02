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
edit_user_config() #create a config file
#> User configuration loaded from file:
#> ~/.local/share/eemanalyzeR/user-config.yaml
#> Changes to user configuration applied.
reset_user_config() #reset config file
#> User configuration reset.
#> Find reset conifg at ~/.local/share/eemanalyzeR/user-config.yaml
load_user_config() #load config file
#> User configuration loaded from file:
#> ~/.local/share/eemanalyzeR/user-config.yaml
```
