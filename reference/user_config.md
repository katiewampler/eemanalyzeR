# Set up and apply user defaults for data processing

Reads the user generated YAML file (`user-config.yaml`) which stores the
user specified values for the arguments in
[`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
and applies them to the package environment (`.pkgenv`). This allows the
user to specify processing parameters that are maintained across R
sessions.

## Usage

``` r
edit_user_config()

load_user_config(
  config_path = rappdirs::user_data_dir("eemanalyzeR"),
  env = .pkgenv
)
```

## Arguments

- config_path:

  path the YAML file with user default values

- env:

  the environment name to write to

## Value

- **edit_user_config** opens up `user-config.yaml`.

- **load_user_config** will apply the user defaults from
  `user-config.yaml` to the package environment

## Details

The defaults are stored in a YAML configuration file on the user data
directory. This function will open up the file so the text can be
edited. To save new defaults simply edit the file and save it. The
arguments in this file will overwrite the defaults set in the package.

## Examples

``` r
edit_user_config()
#> User configuration loaded from file:
#> ~/.local/share/eemanalyzeR/user-config.yaml
#> Changes to user configuration applied.

load_user_config()
#> User configuration loaded from file:
#> ~/.local/share/eemanalyzeR/user-config.yaml
```
