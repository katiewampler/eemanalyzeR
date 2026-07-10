# Default configuration of arguments for eemanalyzeR

The default argument values used in the
[`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
function. Argument values are stored in a package environment
(`.pkgenv`) which will be accessed in the case where a specific variable
is not specified by the user.

## Usage

``` r
default_config
```

## Format

A list of length 33:

- **abs_pattern**: Used by
  [`abs_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  A character string containing a
  [`base::regular expression()`](https://rdrr.io/r/base/regex.html) to
  match files in `input_dir`. Only files matching the pattern will be
  loaded.

- **abs_skip**: Used by
  [`abs_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  A character string containing a
  [`base::regular expression()`](https://rdrr.io/r/base/regex.html) to
  match files in `input_dir` that should be ignored.

- **abs_file_ext**: Used by
  [`abs_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  The file extension of the absorbance files.

- **abs_recurse_read**: Used by
  [`eem_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  Logical. Should the function recursively search directories?

- **eem_pattern**: Used by
  [`eem_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  A character string containing a
  [`base::regular expression()`](https://rdrr.io/r/base/regex.html) to
  match files in `input_dir`. Only files matching the pattern will be
  loaded.

- **eem_skip**: Used by
  [`eem_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  A character string containing a
  [`base::regular expression()`](https://rdrr.io/r/base/regex.html) to
  match files in `input_dir` that should be ignored.

- **eem_file_ext**: Used by
  [`eem_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  The file extension of the EEMs files.

- **eem_recurse_read**: Used by
  [`eem_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  Logical. Should the function recursively search directories?

- **eem_import_func**: Used by
  [`abs_dir_read()`](https://katiewampler.github.io/eemanalyzeR/reference/dir_read.md).
  Character or a user-defined function to import an EEM. For more
  details, see
  [`vignette("custom-indices")`](https://katiewampler.github.io/eemanalyzeR/doc/custom-indices.md).

- **meta_file**: Used by
  [`meta_read()`](https://katiewampler.github.io/eemanalyzeR/reference/meta_read.md).
  Name of the metadata file the program is expecting. Defaults to NA,
  which auto-detects the metadata file in the data directory, for
  flexibility.

- **meta_sheet**: Used by
  [`meta_read()`](https://katiewampler.github.io/eemanalyzeR/reference/meta_read.md).
  Name of the sheet containing metadata (only required if the metadata
  is not on the first sheet of an `.xlsx` file).

- **meta_validate**: Used by
  [`meta_read()`](https://katiewampler.github.io/eemanalyzeR/reference/meta_read.md).
  Logical. If `TRUE`, checks the metadata for structural issues that
  could cause problems during processing. Recommended to keep `TRUE`.

- **iblank_pattern**: Used by
  [`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).
  A character vector of length 1 with a regular expression that matches
  sample names of instrument blanks.

- **sblank_pattern**: Used by
  [`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).
  A character vector of length 1 with a regular expression that matches
  sample names of sample blanks.

- **check_pattern**: Used by
  [`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).
  A character vector of length 1 with a regular expression that matches
  sample names of check standards.

- **blank_validate**: Used by
  [`add_blanks()`](https://katiewampler.github.io/eemanalyzeR/reference/add_blanks.md).
  Logical vector length one indicating whether blanks should be
  validated.

- **ex_clip**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Numeric vector of length two specifying the minimum and maximum
  excitation wavelengths to keep.

- **em_clip**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Numeric vector of length two specifying the minimum and maximum
  emission wavelengths to keep.

- **type**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Logical vector of length four indicating which scattering lines to
  remove. The order is "raman1", "raman2", "rayleigh1", "rayleigh2".

- **width**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Numeric vector of length four specifying the width of scattering lines
  to remove (nm). Same order as `type`.

- **interpolate**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Logical vector of length four indicating which scattering lines to
  interpolate. Same order as `type`.

- **method**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Numeric (0–4) specifying the interpolation method to use. Default
  is 1. See
  [`staRdom::eem_interp()`](https://rdrr.io/pkg/staRdom/man/eem_interp.html)
  for details.

- **cores**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Integer specifying the number of cores for parallel computation during
  interpolation.

- **cuvle**: Used by
  [`process_eem()`](https://katiewampler.github.io/eemanalyzeR/reference/process_eem.md).
  Cuvette (path) length in cm.

- **index_method**: Used by
  [`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md).
  Either "eemanalyzeR", "eemR", "usgs", or a custom function.

- **tolerance**: Used by
  [`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md).
  Maximum percent deviation that the check standard can vary from the
  long-term values without being flagged.

- **return**: Used by
  [`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md).Output
  format: "long" or "wide".

- **qaqc_dir**: Used by
  [`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md).
  File path to the QAQC files generated with
  [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
  and
  [`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md).
  Default is a platform- and user-dependent data directory
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html)
  created when QAQC files are generated.

- **qaqc_method**:

- **filename**: Used by
  [`export_data()`](https://katiewampler.github.io/eemanalyzeR/reference/export_data.md).
  A character string, used to write output file names.

- **output_dir**: Used by
  [`export_data()`](https://katiewampler.github.io/eemanalyzeR/reference/export_data.md).
  Path to save the data. Defaults to NA, which saves to a temporary
  directory if not specified.

- **spectra_to_csv**: Used by
  [`export_data()`](https://katiewampler.github.io/eemanalyzeR/reference/export_data.md).
  Logical. If `TRUE`, processed EEM and absorbance data and metadata are
  written to `output_dir` as `.csv` files.

- **sum_plot**: Used by
  [`export_data()`](https://katiewampler.github.io/eemanalyzeR/reference/export_data.md).
  Logical. If `TRUE`, individual plots are exported as a single large
  figure along with the individual plots.

- **package_version**: the version of eemanalyzeR currently installed on
  the system.

## Details

These values can be edited by:

- the user permanently using
  [`edit_user_config()`](https://katiewampler.github.io/eemanalyzeR/reference/user_config.md)

- the user for the current R session using
  [`modify_session_config()`](https://katiewampler.github.io/eemanalyzeR/reference/modify_session_config.md)

- or within the
  [`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
  function itself by providing argument values
