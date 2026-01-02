# Add metadata to absorbance and EEM data

Adds metadata from a data frame to an `abslist` or `eemlist` object as
additional list items.

## Usage

``` r
add_metadata(
  meta,
  x,
  iblank_pattern = "BEM$|Waterfall ?Plot ?Blank",
  sblank_pattern = "Blank|blank|BLK",
  check_pattern = "Tea|tea"
)
```

## Arguments

- meta:

  A `data.frame` containing metadata.

- x:

  An `eemlist` or `abslist` object.

- iblank_pattern:

  a character vector of length 1 with a regular expression that matches
  sample names of instrument blanks. Default is "BEM\$\|Waterfall ?Plot
  ?Blank"

- sblank_pattern:

  a character vector of length 1 with a regular expression that matches
  sample names of sample blanks. Default is "Blank\|blank\|BLK"

- check_pattern:

  a character vector of length 1 with a regular expression that matches
  sample names of check standards. Default is "Tea\|tea"

## Value

An object of the same class as the input (`abslist` or `eemlist`), with
metadata added.

For each sample, the following fields may be added (if present in the
metadata):

- **meta_name**: identifier for the sample

- **dilution**: sample dilution factor

- **analysis_date**: date the sample was run

- **description**: optional description

- **sample_type**: optional flag (e.g., `sample` for a sample, `sblank`
  for an analytical blank, `check` for a check standard). Default values
  match Horiba Aqualog exports.

- **doc_mgL**: dissolved organic carbon concentration in mg/L

- **notes**: optional notes on sample or sampling conditions

If `x` is an `eemlist`, two additional items are added:

- **integration_time_s**: integration time of the sample

- **raman_area_1s**: Raman water peak area normalized to 1-second
  integration time.

## Note

If an `eemlist` contains blanks, the blanks automatically inherit
metadata from their corresponding sample.

## Examples

``` r
# Add metadata to absorbance data
abs_augment <- add_metadata(metadata, example_abs)

# Add metadata to EEM data
eem_augment <- add_metadata(metadata, example_eems)
```
