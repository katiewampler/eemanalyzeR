# Nicely combine data QAQC flags

If the previous flags were \`NA\`, replaces with the new flagged value,
otherwise combines the flags with a "\_" between.

## Usage

``` r
.combine_flags(x, x1, mdl = FALSE)
```

## Arguments

- x:

  existing flags

- x1:

  flags to add

- mdl:

  logical, combing two MDL flags?

## Value

A character vector of combined flags.

## Examples

``` r
.combine_flags("DATA01", NA)
#> [1] "DATA01"
.combine_flags(NA, "MDL01")
#> [1] "MDL01"
.combine_flags(NA, NA)
#> [1] NA
.combine_flags("DATAO1", "MDL01")
#> [1] "DATAO1_MDL01"
.combine_flags("DATA01", "DATA01")
#> [1] "DATA01"
```
