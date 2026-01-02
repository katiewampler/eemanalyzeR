# Load, clean, and process EEMs and absorbance data

The main high-level function of the
[eemanalyzeR](https://katiewampler.github.io/eemanalyzeR/reference/eemanalyzeR-package.md)
package. It is designed for users who want a fast, automated workflow
without needing to write complex R code or string together a bunch of
processing functions. By pointing the function to a folder of raw EEMs
and absorbance data, the function performs the full processing pipeline
from start to finish.

## Usage

``` r
run_eems(
  input_dir,
  output_dir = NA,
  filename = NA,
  interactive = TRUE,
  blanklist = NULL,
  qaqc_checks = TRUE,
  ...
)
```

## Arguments

- input_dir:

  Path to folder containing raw EEMs and/or absorbance files.

- output_dir:

  Path to save the processed data.

- filename:

  A character string, used for file names.

- interactive:

  Logical, used to determine if user input should be used.

- blanklist:

  `eemlist` of blank files to subtract from samples. Automatically uses
  instrument blanks if not provided.

- qaqc_checks:

  Logical, should user QA/QC files be used to check data?

- ...:

  additional arguments used to make one time modifications to processing
  arguments and plotting
  ([plot.eemlist](https://katiewampler.github.io/eemanalyzeR/reference/plot.md)).
  List of optional arguments to modify within the processing are located
  in
  [default_config](https://katiewampler.github.io/eemanalyzeR/reference/default_config.md).
  See details for more info on modifying processing configuration.

## Value

The following files are saved to the output directory specified. The
list contains:

- **eemlist:** the `eemlist`

- **abslist:** the `abslist`

- **readme:** a character vector containing information about the data
  processing steps

- **meta:** the metadata associated with the samples, may be `NULL` if
  not provided

- **indices:** a list of EEMs and absorbance indices, may be `NULL` if
  not provided

- **eem_plot:** a list of EEMs plots, may be `NULL` if not provided

- **abs_plot:** a ggplot2 object of the absorbance data, may be `NULL`
  if not provided

## Details

The only requirement to use this wrapper function to process EEMs and
absorbance data is an **input directory** containing the raw data files
and a metadata spreadsheet. However, there are many optional arguments
for the user to modify the EEMs and absorbance processing. The names and
default values for these arguments can be found in the documentation for
the
[default_config](https://katiewampler.github.io/eemanalyzeR/reference/default_config.md).

There are four ways for user to use and modify the processing
configuration defaults.

- **Option 1:** User doesn't change anything, the package defaults are
  used by `run_eems`.

- **Option 2:** User creates a file (stored on their computer) that has
  processing defaults that eemanalyzeR pulls from at load time. This is
  created using the
  [`edit_user_config()`](https://katiewampler.github.io/eemanalyzeR/reference/user_config.md)
  function.

- **Option 3:** User modifies the defaults *BEFORE* using the `run_eems`
  function using
  [`modify_config()`](https://katiewampler.github.io/eemanalyzeR/reference/modify_config.md)
  function. This modifies the settings for the R session and will be
  applied to any data processing that occurs until the package is
  reloaded or the R session is restarted. After the package is reloaded
  the defaults revert back to the package defaults or (if they exist)
  user defaults.

- **Option 4:** User supplies arguments to `run_eems` function that
  modify processing *ONLY* during that run. These configuration options
  will not persist across multiple tries of `run_eems` and must be
  specified each time, but the argument values will be reported in the
  readme file.

## Note

Currently `run_eems` does not allow for custom import or index
functions.
