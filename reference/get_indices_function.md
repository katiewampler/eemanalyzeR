# Get function to generate fluorescence and absorbance indices

Returns either a user-supplied custom function or one of the preset
index generators.

## Usage

``` r
get_indices_function(index_method = "eemanalyzeR")
```

## Arguments

- index_method:

  Either "eemanalyzeR", "eemR", "usgs", or a custom function.

## Value

A function used to generate indices.

## Details

Preset methods correspond to the following functions:

- **eemanalyzeR**:
  [`eemanalyzeR_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/eemanalyzeR_indices.md)

- **eemR**:
  [`eemR_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/eemR_indices.md)

- **usgs**:
  [`usgs_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/usgs_indices.md)
