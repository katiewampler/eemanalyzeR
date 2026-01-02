# Read metadata from a file

Reads a `.csv` or `.xlsx` file containing metadata for absorbance and
EEM data. See
[metadata](https://katiewampler.github.io/eemanalyzeR/reference/metadata.md)
for required columns and structure.

## Usage

``` r
meta_read(input_dir, meta_file = NA, sheet = NA, validate_metadata = TRUE)
```

## Arguments

- input_dir:

  Path to the metadata file or the folder containing it.

- meta_file:

  Optional: Filename of the metadata file. If left as NA, the metadata
  file will be guessed from files in the input directory.

- sheet:

  Name of the sheet containing metadata (only required if the metadata
  is not on the first sheet of an `.xlsx` file).

- validate_metadata:

  Logical. If `TRUE`, checks the metadata for structural issues that
  could cause problems during processing. Recommended to keep `TRUE`.

## Value

A `data.frame` containing sample metadata.

## Examples

``` r
metadata <- meta_read(system.file("extdata", package = "eemanalyzeR"))
#> No Meta file specified, using:
#> /home/runner/work/_temp/Library/eemanalyzeR/extdata/metadata_example.csv
```
