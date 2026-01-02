# Check if a check standard is consistent with long-term standard

Calculate indices for a check standard to ensure they are consistent
with the long-term standard values.

## Usage

``` r
check_std(
  eemlist,
  abslist,
  qaqc_dir = .qaqc_dir(),
  tolerance = 0.2,
  index_method = "eemanalyzeR",
  vals = FALSE
)
```

## Arguments

- eemlist:

  An `eemlist` object.

- abslist:

  An `abslist` object.

- qaqc_dir:

  File path to the QAQC files generated with
  [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
  and
  [`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md).
  Default is a user-specific data directory
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html).

- tolerance:

  Maximum percent deviation that the check standard can vary from the
  long-term values without being flagged.

- index_method:

  Either "eemanalyzeR", "eemR", "usgs", or a custom function.

- vals:

  If `TRUE`, returns actual values, otherwise returns just flags.

## Value

If `vals = FALSE`, returns a `data.frame` with four columns:

- `meta_name`: metadata name of the check standard

- `type`: `abs` or `eem` indicating the index type

- `index`: name of the index

- `flag`: "STD01" if outside tolerance, otherwise `NA`

If `vals = TRUE`, additional columns include:

- `observed`: observed index value

- `standard`: long-term standard value

- `percent_deviation`: percent difference from the standard

## Details

`eemlist` and `abslist` must be fully processed to match the long-term
standard.

## See also

[`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md)

## Examples

``` r
check_std(
  example_processed_eems,
  example_processed_abs,
  qaqc_dir = system.file("extdata", package = "eemanalyzeR")
)
#>           meta_name type    index  flag
#> 1     ExampleTeaStd  eem      BIX  <NA>
#> 2  ManualExampleTea  eem      BIX  <NA>
#> 3     ExampleTeaStd  eem       FI  <NA>
#> 4  ManualExampleTea  eem       FI  <NA>
#> 9     ExampleTeaStd  eem    fresh  <NA>
#> 10 ManualExampleTea  eem    fresh  <NA>
#> 15    ExampleTeaStd  eem       pB  <NA>
#> 16 ManualExampleTea  eem       pB STD01
#> 19    ExampleTeaStd  eem       pC  <NA>
#> 20 ManualExampleTea  eem       pC STD01
#> 23    ExampleTeaStd  eem       pD  <NA>
#> 24 ManualExampleTea  eem       pD STD01
#> 31    ExampleTeaStd  eem       pM  <NA>
#> 32 ManualExampleTea  eem       pM STD01
#> 35    ExampleTeaStd  eem       pN  <NA>
#> 36 ManualExampleTea  eem       pN STD01
#> 39    ExampleTeaStd  eem       pT  <NA>
#> 40 ManualExampleTea  eem       pT STD01
#> 47    ExampleTeaStd  eem      rCM  <NA>
#> 48 ManualExampleTea  eem      rCM  <NA>
#> 49    ExampleTeaStd  eem      rCT  <NA>
#> 50 ManualExampleTea  eem      rCT  <NA>
#> 51    ExampleTeaStd  abs    E2_E3  <NA>
#> 52 ManualExampleTea  abs    E2_E3  <NA>
#> 55    ExampleTeaStd  abs S275_295  <NA>
#> 56 ManualExampleTea  abs S275_295  <NA>
#> 57    ExampleTeaStd  abs S350_400  <NA>
#> 58 ManualExampleTea  abs S350_400  <NA>
#> 59    ExampleTeaStd  abs       SR  <NA>
#> 60 ManualExampleTea  abs       SR  <NA>
```
