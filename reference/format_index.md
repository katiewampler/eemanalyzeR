# Format index and flag data

Combines index values, QA/QC flags, and sample metadata into a clean
`data.frame` for further processing. Any values flagged as "MDL01"
(below detection limit) are removed and not reported.

## Usage

``` r
format_index(x, index, value, flag)
```

## Arguments

- x:

  An `eemlist` or `abslist` object.

- index:

  A character string giving the name of the index.

- value:

  Index values. Use
  [`get_fluorescence()`](https://katiewampler.github.io/eemanalyzeR/reference/get_fluorescence.md)
  or
  [`get_absorbance()`](https://katiewampler.github.io/eemanalyzeR/reference/get_absorbance.md)
  to generate these values.

- flag:

  Flag values, the same length as `value`.

## Value

A data frame with four columns:

- **sample_name**: the sample name

- **meta_name**: the sample name in metadata (if provided), otherwise
  repeats `sample_name`

- **index**: the name of the index

- **value**: the index value (with `MDL01` values removed)

## Examples

``` r
ex <- 240:260
em <- 300:320

vals <- get_fluorescence(example_eems, ex, em, stat = "max")
flags <- flag_missing(example_eems, ex = ex, em = em, all = FALSE)

index_formatted <- format_index(
  x = example_eems,
  index = "test_index",
  value = vals,
  flag = flags
)
```
