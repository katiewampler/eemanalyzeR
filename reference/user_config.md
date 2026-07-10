# Set up and apply user defaults for data processing

Reads the user generated YAML file (`user-config.yaml`) which stores the
user specified values for the arguments in
[`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
and applies them to the processing session. This allows the user to
specify processing parameters that are maintained across R sessions.

## Usage

``` r
edit_user_config(
  ...,
  interactive = TRUE,
  user_config_file = .user_config_path()
)

reset_user_config(user_config_file = .user_config_path())

read_user_config(user_config_file = .user_config_path())

validate_user_config(user_config_file = .user_config_path())

load_user_config(user_config_file = .user_config_path(), env = .pkgenv)

repair_user_config(user_config_file = .user_config_path())
```

## Arguments

- ...:

  for `edit_user_config` potential parameter names and values to apply
  to the user config. Only if interactive = FALSE

- interactive:

  for `edit_user_config` defaults to TRUE, which will open the user
  config file for manual editing. If FALSE, it attempts to apply the
  values provided in `...`

- user_config_file:

  The path to the file that stores the user config. Defaults to the yaml
  file in the default user data directory.

- env:

  the environment name to write to (defaults to the package environment)

## Value

- **edit_user_config** returns a message that the user configuration has
  been edited.

- **reset_user_config** invisibly returns the reset default
  configuration settings as a named list.

- **read_user_config** invisibly returns the current user configuration
  as a named list.

- **validate_user_config** invisibly returns TRUE if the configuration
  is valid, otherwise returns an error

- **load_user_config** will apply the options from `user-config.yaml` to
  the current session.

- **repair_user_config** invisibly returns the repaired user config

## Details

- **edit_user_config** opens up `user-config.yaml` for manual editing or
  applies the named arguments supplied as `...` to the user config file.

- **reset_user_config** overwrites the data processing options in the
  user configuration file back to the default configuration of the
  eemanalyzeR package. These defaults are documented in data.R under
  "default_config". This function is provided in case the user has a
  malformed configuration file or wants to revert back to default
  processing settings after experimenting with modifying the settings
  using `edit_user_config`.

- **read_user_config** will read the options from the user config path
  as an R object but does not apply to the session. This function is
  used as a helper to read what's in the stored user configuration file
  and return it as a list before validating the config and applying it
  to the current session. Usually the user wants to use
  `load_user_config` since that function reads the config, checks it,
  then applies it to the current session.

- **validate_user_config** reads the user config and checks the options
  are valid and warns the user about invalid options. It checks that all
  settings in the eemanalyzeR user config are valid options that match
  the template included with the package.

- **load_user_config** will apply the options from `user-config.yaml` to
  the current session.

- **repair_user_config** will attempt to repair the user configuration
  file by merging the valid user configuration file with the default
  configuration file.

## Examples

``` r
load_user_config()
#> User configuration read from file:
#> ~/.local/share/eemanalyzeR/user-config.yaml
```
