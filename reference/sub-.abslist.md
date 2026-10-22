# Subsetting using `[` for an abslist

Subsetting using `[` for an abslist

## Usage

``` r
# S3 method for class 'abslist'
abslist[i]
```

## Arguments

- abslist:

  the abslist to subset

- i:

  the index for subsetting

## Value

an object of class `abslist`

## Examples

``` r
abs <- example_abs[1] #can get a single absorbance (still as abslist)

abs_multiple <- example_abs[1:3] #or get several
```
