# Convert between an `eem` object and a `data.frame`

Sometimes it is easier to subset an EEM matrix if it is in a long
format. This function will take an EEM matrix and turn it into a
`data.frame` or, if it is a `data.frame`, convert it to an `eem` object.

## Usage

``` r
eem_transform(eem, file = NULL, sample = NULL, location = NULL)
```

## Arguments

- eem:

  An object of class `eem` or a `data.frame` with three columns: `ex`,
  `em`, and `fluor`.

- file:

  Optional. File path to the EEM data including the data file name; only
  used if `eem` is a `data.frame`.

- sample:

  Optional. The name of the EEM sample; only used if `eem` is a
  `data.frame`.

- location:

  Optional. The path to the directory where the file is located; only
  used if `eem` is a `data.frame`.

## Value

Converts between an object of class `eem` and a `data.frame` with three
columns:

- `ex`: the excitation wavelengths

- `em`: the emission wavelengths

- `fluor`: the fluorescence values

## Details

If you're loading samples that have been partially processed, it's
recommended to set the correct processing attributes to `TRUE` to
indicate these steps have already been performed. Use `attributes(eem)`
to see attributes, and `attr(eem, "attribute_name") <- TRUE` to set
them.

## Examples

``` r
# Convert an EEM to a long data.frame
flat_eem <- eem_transform(example_eems[[1]])

# Convert back to an EEM object
eem_obj <- eem_transform(flat_eem)
```
