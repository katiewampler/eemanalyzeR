# Get fluorescence and absorbance indices

Calculates commonly used indices from absorbance and excitation–emission
matrix (EEM) data. Also checks and flags index values for potential
issues.

## Usage

``` r
get_indices(
  eemlist,
  abslist,
  index_method = "eemanalyzeR",
  tolerance = 0.2,
  return = "wide",
  cuvle = 1,
  qaqc_dir = NA,
  arg_names = NULL
)
```

## Arguments

- eemlist:

  An `eemlist` object.

- abslist:

  An `abslist` object.

- index_method:

  Either "eemanalyzeR", "eemR", "usgs", or a custom function. See
  **Details**.

- tolerance:

  Maximum percent deviation that the check standard can vary from the
  long-term values without being flagged.

- return:

  Output format: "long" or "wide".

- cuvle:

  Cuvette (path) length in cm.

- qaqc_dir:

  File path to the QAQC files generated with
  [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
  and
  [`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md).
  Default is a user-specific data directory
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html).

- arg_names:

  Optional list of arguments passed from higher-level functions for
  README generation.

## Value

A list with two data frames:

- **eem_index**: fluorescence indices

- **abs_index**: absorbance indices

### Long format

If `return = "long"`, each data frame includes:

- `sample_name`: sample ID

- `meta_name`: sample name in metadata (or `sample_name` if missing)

- `index`: index name

- `value`: index value

- `QAQC_flag`: any associated flags

### Wide format

If `return = "wide"`, each row corresponds to a sample, and columns
represent indices. Flags appear:

- *alone*, if no numeric value could be returned

- in the form VALUE_FLAG when a value exists

## Details

### Index methods

Three preset index sets are available:

- **eemanalyzeR**:
  [`eemanalyzeR_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/eemanalyzeR_indices.md)

- **eemR**:
  [`eemR_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/eemR_indices.md)

- **usgs**:
  [`usgs_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/usgs_indices.md)

You may also pass a custom index-generating function. See the
[custom-indices
vignette](https://katiewampler.github.io/eemanalyzeR/doc/custom-indices.md)
for instructions.

### QA/QC flags

Index values are checked for common issues. Flags include:

- **DATA01**: Missing data required for calculation

- **DATA02**: Missing required wavelengths; value may be inaccurate

- **DATA03**: Ratio denominator was zero

- **DATA04**: Spectral slope could not be calculated

- **DOC01**: Missing dissolved organic carbon (DOC) data

- **INF01**: Infinite value

- **MDL01**: All values below MDL

- **MDL02**: One or more values below MDL; use cautiously

- **MDL03**: Ratio index where numerator or denominator was entirely
  below MDL

- **NEG01**: Negative value

- **STD01**: Check standard value outside tolerance

- **VAL01**: Value below typical range

- **VAL02**: Value above typical range

## Examples

``` r
indices <- get_indices(
  example_processed_eems,
  example_processed_abs,
  qaqc_dir = system.file("extdata", package = "eemanalyzeR")
)
```
