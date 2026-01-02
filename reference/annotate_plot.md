# Annotate EEM plot with location of indices

Adds labels indicating the regions of an excitation–emission matrix
(EEM) that are used to calculate fluorescence indices. The annotation is
customized according to the selected index method.

## Usage

``` r
annotate_plot(plot, index_method = "eemanalyzeR")
```

## Arguments

- plot:

  A `ggplot2` object of an `eem`.

- index_method:

  Either "eemanalyzeR", "eemR", "usgs".

## Value

A `ggplot2` object.

## Examples

``` r
plot <- plot(example_processed_eems[[3]])
annotate_plot(plot)
```
