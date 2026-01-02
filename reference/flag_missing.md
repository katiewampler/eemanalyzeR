# QA/QC flags for missing data

Checks if a metric can't be calculated or may be inaccurate due to
missing wavelengths required to calculate that metric.

## Usage

``` r
flag_missing(x, ex = NULL, em = NULL, wl = NULL, all = TRUE)
```

## Arguments

- x:

  An `eemlist`, `eem`, `abslist`, or `abs` object.

- ex:

  A vector of excitation wavelengths required to calculate the index.
  Only needed if `x` is an `eemlist` or `eem`.

- em:

  A vector of emission wavelengths required to calculate the index. Only
  needed if `x` is an `eemlist` or `eem`.

- wl:

  A vector of wavelengths required to calculate the index. Only needed
  if `x` is an `abslist` or `abs`.

- all:

  If `TRUE` index will not be calculated if some wavelengths are
  missing, returning "DATA01".

## Value

A vector containing text flags and `NA` values. Possible values:

- "DATA_01": Missing data required to calculate the index.

- "DATA_02": Missing some wavelengths required to calculate the index;
  value may be inaccurate.

- `NA`: No missing data; no flag needed.

## Examples

``` r
# checking absorbance data
# data exists
flag_missing(example_abs, wl = 400)
#> [1] NA NA NA NA

# data doesn't exist
flag_missing(example_abs, wl = 100)
#> [1] "DATA01" "DATA01" "DATA01" "DATA01"

# some data exists, still calculate
flag_missing(example_abs, wl = 100:254, all = FALSE)
#> [1] "DATA02" "DATA02" "DATA02" "DATA02"

# checking fluorescence data
# data exists
flag_missing(example_eems, ex = 270:280, em = 300:320)
#> [1] NA NA NA NA NA NA NA NA

# data doesn't exist
flag_missing(example_eems, ex = 100:150, em = 300:320)
#> [1] "DATA01" "DATA01" "DATA01" "DATA01" "DATA01" "DATA01" "DATA01" "DATA01"

# some data exists, still calculate
flag_missing(example_eems, ex = 100:350, em = 300:320, all = FALSE)
#> [1] "DATA02" "DATA02" "DATA02" "DATA02" "DATA02" "DATA02" "DATA02" "DATA02"
```
