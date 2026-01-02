# Get fluorescence within a specified range

Gets fluorescence within a range of excitation and emission wavelengths
and returns either the maximum value or the sum across that range.
Missing values are interpolated using
[`pracma::interp2()`](https://rdrr.io/pkg/pracma/man/interp2.html).

## Usage

``` r
get_fluorescence(eem, ex, em, stat = "max", norm = FALSE)
```

## Arguments

- eem:

  An `eem` or `eemlist` object.

- ex:

  A vector of excitation wavelengths.

- em:

  A vector of emission wavelengths.

- stat:

  A string specifying the statistic to return:

  - "max": return the maximum value (default)

  - "sum": return the sum of values

- norm:

  Logical. If `TRUE`, divides the index value by the DOC concentration.
  Metadata must be added using
  [`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md),
  otherwise `NA` will be returned.

## Value

A numeric vector of fluorescence values. Returns `NA` if values cannot
be extracted.

## Examples

``` r
pA <- get_fluorescence(example_eems, ex = 250:260, em = 380:480)
pD <- get_fluorescence(example_eems, ex = 390, em = 509)

pA_sum <- get_fluorescence(
  example_eems,
  ex = 250:260,
  em = 380:480,
  stat = "sum"
)

eemlist <- add_metadata(metadata, example_eems)

pA_docnorm <- get_fluorescence(
  eemlist,
  ex = 250:260,
  em = 380:480,
  norm = TRUE
)
```
