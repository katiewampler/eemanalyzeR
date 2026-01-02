# Subset `eemlist` or `abslist` based on components

Helper function to select or remove samples from an `eemlist` or
`abslist` using components beyond just the sample name. Builds on
[`eemR::eem_extract()`](https://rdrr.io/pkg/eemR/man/eem_extract.html).

## Usage

``` r
subset_samples(
  x,
  info,
  sample,
  keep = FALSE,
  ignore_case = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  An `eemlist` or `abslist` object.

- info:

  Name of the component within the `eem` or `abs` to extract. See
  [eemR::eem](https://rdrr.io/pkg/eemR/man/eem.html) for base `eem`
  components and
  [`abs_read()`](https://katiewampler.github.io/eemanalyzeR/reference/abs_read.md)
  for base `abs` components. Extended components may be added with
  [`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).

- sample:

  A vector of names or other component values to select/exclude from
  `x`.

- keep:

  Logical. If `TRUE`, returns the specified samples; if `FALSE`, removes
  them. Default is `FALSE`.

- ignore_case:

  Logical. Should case be ignored when matching? Default is `FALSE`.

- verbose:

  Logical. If `TRUE`, prints removed or extracted samples to the
  console.

## Value

An object of class `eemlist` or `abslist` with samples selected or
removed based on `sample`.

## See also

[`get_sample_info()`](https://katiewampler.github.io/eemanalyzeR/reference/get_sample_info.md)

## Examples

``` r
# Subset by sample name
names <- get_sample_info(example_eems, "sample")
eem_subset <- subset_samples(example_eems, "sample", names[1]) # removes by default
#> Removed sample(s):
#> B1S1ExampleBlankBEM
eem_subset <- subset_samples(example_eems, "sample", names[1], keep = TRUE) # keeps instead
#> Extracted sample(s):
#> B1S1ExampleBlankBEM

# Subset by metadata name
eemlist <- add_metadata(metadata, example_eems)
names <- get_sample_info(eemlist, "meta_name")
eem_subset <- subset_samples(eemlist, "meta_name", names[1]) # removes by default
#> Removed sample(s):
#> ExampleBlank
#> ExampleBlank
```
