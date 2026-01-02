# Perform blank subtraction on EEM samples

Subtracts the blank EEM data from sample EEM data. Samples must have
metadata and blank data attached prior to running.

## Usage

``` r
subtract_blank(eem)
```

## Arguments

- eem:

  An `eem` or `eemlist` object.

## Value

An object of class `eem` or `eemlist` where the sample data (`x`) has
been subtracted by the blank data (`blk_x`).

## Details

Blank subtraction requires that blank EEM data be attached to each
sample. This is done with
[`add_blanks()`](https://katiewampler.github.io/eemanalyzeR/reference/add_blanks.md),
which in turn requires metadata to be added via
[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).

## See also

[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md)

## Examples

``` r
eem <- add_metadata(metadata, example_eems)
blanklist <- subset_type(eem, "iblank")
eem <- add_blanks(eem, blanklist)

# Subtract blank from a single EEM
eem_sub <- subtract_blank(eem[[1]])

# Subtract blank from an EEM list
eemlist_sub <- subtract_blank(eem)
```
