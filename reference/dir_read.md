# Read absorbance and fluorescence data from directory

Wrappers for
[`eemR::eem_read()`](https://rdrr.io/pkg/eemR/man/eem_read.html) and
[`abs_read()`](https://katiewampler.github.io/eemanalyzeR/reference/abs_read.md)
to read all EEMs or absorbance files in a directory into R, even when
the directory contains other files.

## Usage

``` r
eem_dir_read(
  input_dir,
  pattern = NA,
  skip = "(?i)abs",
  file_ext = "dat",
  recursive = FALSE,
  import_function = "aqualog",
  verbose = TRUE
)

abs_dir_read(
  input_dir,
  pattern = NA,
  skip = "SEM|BEM|Waterfall",
  file_ext = "dat",
  recursive = FALSE,
  verbose = TRUE
)
```

## Arguments

- input_dir:

  Path to folder containing raw EEMs and/or absorbance files.

- pattern:

  Optional. A character string containing a
  [`base::regular expression()`](https://rdrr.io/r/base/regex.html) to
  match files in `input_dir`. Only files matching the pattern will be
  loaded.

- skip:

  Optional. A character string containing a
  [`base::regular expression()`](https://rdrr.io/r/base/regex.html) to
  match files in `input_dir` that should be ignored.

- file_ext:

  The file extension of the EEMs or absorbance files.

- recursive:

  Logical. Should the function recursively search directories?

- import_function:

  Character or a user-defined function to import an EEM. For more
  details, see
  [[`vignette("custom-indices")`](https://katiewampler.github.io/eemanalyzeR/articles/custom-indices.md)](https://katiewampler.github.io/eemanalyzeR/doc/custom-indices.md).

- verbose:

  Logical. Should the function return notes about resetting the readme
  file?

## Value

- `eem_dir_read()` returns an object of class `eemlist`, containing a
  list of `eem`. For more details see
  [`eemR::eem_read()`](https://rdrr.io/pkg/eemR/man/eem_read.html).

- `abs_dir_read()` returns an object of class `abslist`, containing a
  list of `abs`. For more details see
  [`abs_read()`](https://katiewampler.github.io/eemanalyzeR/reference/abs_read.md).

## Examples

``` r
# Load all EEMs from directory
eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"))
#> NOTE: removed previous 'readme' file

# Load all EEMs matching a pattern from directory
eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "SEM")

# Load absorbance samples using a pattern
abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "abs|ABS")

# Load absorbance samples while skipping EEMs and other files
abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "SEM|BEM|waterfall")
#> Warning: Unable to import file: /home/runner/work/_temp/Library/eemanalyzeR/extdata/ManualExampleTeaWaterfallPlotBlank.dat.
#> Please use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.
#> Warning: Unable to import file: /home/runner/work/_temp/Library/eemanalyzeR/extdata/ManualExampleTeaWaterfallPlotBlank.dat.
#> Please use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.
#> Warning: Unable to import file: /home/runner/work/_temp/Library/eemanalyzeR/extdata/ManualExampleTeaWaterfallPlotSample.dat.
#> Please use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.
#> Warning: Unable to import file: /home/runner/work/_temp/Library/eemanalyzeR/extdata/ManualExampleTeaWaterfallPlotSample.dat.
#> Please use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.
```
