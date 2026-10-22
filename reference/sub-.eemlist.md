# Subsetting using `[` for eemlist

Subsetting using `[` for eemlist

## Usage

``` r
# S3 method for class 'eemlist'
eemlist[i]
```

## Arguments

- eemlist:

  the eemlist to subset

- i:

  the index for subsetting

## Value

an object of class `eemlist`

## Examples

``` r
eem <- example_eems[1] #can get a single eem (still as eemlist)

eems <- example_eems[1:3] #or get several
```
