# Check whether an EEM or absorbance sample is above the MDL

Checks whether values in an `eem` or `abs` object are above the method
detection limit (MDL) for a specified set of wavelengths or
excitation–emission pairs.

## Usage

``` r
check_eem_mdl(eem, mdl = NULL, ex, em, vals = FALSE)

check_abs_mdl(abs, mdl = NULL, wl, vals = FALSE)
```

## Arguments

- eem:

  An `eem` or `eemlist` object.

- mdl:

  An `eem` or `abs` object containing MDL data. If no MDL is provided,
  `NA` is returned.

- ex:

  A vector of excitation wavelengths.

- em:

  A vector of emission wavelengths.

- vals:

  If `TRUE`, returns actual values, otherwise returns just flags.

- abs:

  An `abs` or `abslist` object.

- wl:

  A vector of absorbance wavelengths.

## Value

If `vals = FALSE`, returns one of:

- "MDL01": all values are **below** the MDL

- "MDL02": some values are **below** the MDL

- `NA`: all values are **above** the MDL

If `vals = TRUE`, returns:

- A `data.frame` containing wavelengths, observed values, and MDL values

## Examples

``` r
# Load MDL data
eem_mdl <- readRDS(file.path(
  system.file("extdata", package = "eemanalyzeR"),
  "eem-mdl.rds"
))

abs_mdl <- readRDS(file.path(
  system.file("extdata", package = "eemanalyzeR"),
  "abs-mdl.rds"
))

# Single EEM sample
check_eem_mdl(example_processed_eems[[1]], eem_mdl,
              ex = 270:280, em = 300:320)
#> [1] "MDL01"

# EEM list
check_eem_mdl(example_processed_eems, eem_mdl,
              ex = 270:280, em = 300:320)
#> [1] "MDL01" NA      NA      NA     

# Single absorbance sample
check_abs_mdl(example_processed_abs[[2]], abs_mdl, wl = 254)
#> [1] NA

# Absorbance list
check_abs_mdl(example_processed_abs, abs_mdl, wl = 254)
#> [1] "MDL01" NA      NA      NA     
```
