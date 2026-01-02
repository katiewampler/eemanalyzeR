# Check blanks and add to sample data

Allows the user to visually check excitation-emission matrix (EEM) plots
of the blanks, and then, if approved, the blanks will be added to the
`eem` objects for further processing.

## Usage

``` r
add_blanks(eemlist, blanklist)
```

## Arguments

- eemlist:

  An `eemlist` object.

- blanklist:

  Optional. An `eem` or `eemlist` containing the blank EEM(s).

## Value

An `eemlist` where each `eem` object has two added components:

- `blk_file`: file path of the blank associated with the sample

- `blk_x`: the blank EEM associated with the sample

## Details

If more than one blank is supplied, the function links each blank to its
sample using the metadata name. These names must match between the
sample and its corresponding blank. Because of this, samples must
already have metadata assigned using
[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).

If a `blanklist` is not provided, one is automatically generated from
the `eemlist` attribute "sample_type", using the samples marked as
"iblank".

## Note

If the instrument blank is not accepted, the function will attempt to
use an analytical blank instead (a sample type 'sblank'). If this blank
is used instead, the blank will be removed from the sample set and a
note will be written to the readme.

## Examples

``` r
eemlist <- add_metadata(metadata, example_eems)
blanklist <- subset_type(eemlist, "iblank")
augment_eemlist <- add_blanks(eemlist, blanklist)
```
