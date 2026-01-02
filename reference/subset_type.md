# Extract samples by type from an `eemlist` or `abslist`

Selects samples based on the `sample_type` attribute added via
[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).
Types include instrument blanks, analytical blanks, check standards, and
regular samples.

## Usage

``` r
subset_type(x, type = c("iblank", "sblank", "check", "sample"), negate = FALSE)
```

## Arguments

- x:

  An `eemlist` or `abslist` object.

- type:

  A character or vector specifying sample type(s) to extract. Options
  include:

  - **iblank**: instrument blank

  - **sblank**: analytical blank

  - **check**: check standard

  - **sample**: regular samples

- negate:

  Logical. If `TRUE`, returns all samples **except** those of the
  specified type(s). Default is `FALSE`.

## Value

An object of the same class as `x` containing the selected (or excluded)
samples.

## Examples

``` r
abs <- add_metadata(metadata, example_abs)
eem <- add_metadata(metadata, example_eems)

# No instrument blanks exist
tea <- subset_type(abs, "iblank")
tea
#> NULL

# Get analytical blanks (tea standards)
tea <- subset_type(abs, "sblank")
get_sample_info(tea, "sample")
#> [1] "B1S1ExampleBlankABS"

# Get all blank EEMs (instrument + analytical)
blk <- subset_type(eem, c("iblank", "sblank"))
get_sample_info(blk, "sample")
#> [1] "B1S1ExampleBlankBEM"                "B1S1ExampleBlankSEM"               
#> [3] "B1S2ExampleTeaStdBEM"               "B1S3ExampleSampleBEM"              
#> [5] "ManualExampleTeaWaterfallPlotBlank"

# Get non-instrument blanks
nonblk <- subset_type(eem, "iblank", negate = TRUE)
get_sample_info(nonblk, "sample")
#> [1] "B1S1ExampleBlankSEM"                 "B1S2ExampleTeaStdSEM"               
#> [3] "B1S3ExampleSampleSEM"                "ManualExampleTeaWaterfallPlotSample"
```
