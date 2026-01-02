# Validate the instrument blank(s)

Plots validation instrument blank samples for manual inspection.

## Usage

``` r
validate_blanks(blanklist)
```

## Arguments

- blanklist:

  An `eemlist` containing containing the blank EEMs.

## Value

A blanklist with validated blanks, or an error if no valid blanks are
selected

## Examples

``` r
eems <- add_metadata(metadata, example_eems)
eems <- subset_type(eems, type = "iblank")
valid_blanklist <- validate_blanks(eems)
```
