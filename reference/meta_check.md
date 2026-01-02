# Run tests to validate metadata

Runs a series of checks to identify missing or malformed metadata and
corrects issues when possible. Ensures that column formats conform to
expectations.

## Usage

``` r
meta_check(meta)
```

## Arguments

- meta:

  A `data.frame` containing sample metadata.

## Value

A corrected metadata `data.frame` with formatting issues resolved where
possible.

## See also

[`meta_read()`](https://katiewampler.github.io/eemanalyzeR/reference/meta_read.md)

## Examples

``` r
metadata <- meta_read(system.file("extdata", package = "eemanalyzeR"))
#> No Meta file specified, using:
#> /home/runner/work/_temp/Library/eemanalyzeR/extdata/metadata_example.csv
metadata <- eemanalyzeR:::meta_check(metadata)
```
