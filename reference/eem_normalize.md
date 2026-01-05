# Normalize an `eem` or `eemlist` based on a normalization factor

Useful for Raman normalization or normalizing to a maximum of one for
blank comparisons.

## Usage

``` r
eem_normalize(eem, factor = NULL)
```

## Arguments

- eem:

  An `eem` or `eemlist` object.

- factor:

  The normalization factor, either a single value or a vector of
  factors. If `NULL`, it will normalize to the maximum value for each
  `eem`.

## Value

An `eem` or `eemlist` where `x` has been normalized.

## Examples

``` r
# Normalize a single EEM
eem_normal <- eem_normalize(example_eems[1])

# Normalize an entire EEM list
eems_normal <- eem_normalize(example_eems)
```
