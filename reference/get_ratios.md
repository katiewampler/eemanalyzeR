# Safely return ratios values

Computes a ratio and returns either the calculated value or a QA/QC flag
if the ratio cannot be computed.

## Usage

``` r
get_ratios(val1, val2)
```

## Arguments

- val1:

  Numerator value(s); may be a single value or a vector.

- val2:

  Denominator value(s); may be a single value or a vector.

## Value

A numeric ratio where possible, otherwise a character QA/QC flag.

## Details

The following conditions are checked before returning a value:

- **DATA_01**: Missing data required to calculate the index

- **DATA_03**: Denominator is zero, so the ratio cannot be calculated

## Examples

``` r
# Calculate the ratio of Peak A to Peak T
pA <- get_fluorescence(example_eems, ex = 250:260, em = 380:480)
pT <- get_fluorescence(example_eems, ex = 270:280, em = 320:350)
rAT <- get_ratios(pA, pT)
```
