# eemR and staRdom methods for fluorescence and absorbance indices

Calculates commonly used absorbance and fluorescence optical indices
from `eemlist` and `abslist` objects using functions from the
[eemR](https://CRAN.R-project.org/package=eemR) and
[staRdom](https://CRAN.R-project.org/package=staRdom) packages. Can
incorporate sample metadata if provided.

## Usage

``` r
eemR_indices(eemlist, abslist, cuvle = 1, qaqc_dir = NULL)
```

## Arguments

- eemlist:

  An `eemlist` object.

- abslist:

  An `abslist` object.

- cuvle:

  Cuvette (path) length in cm.

- qaqc_dir:

  File path to the QAQC files generated with
  [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
  and
  [`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md).
  Default is a user-specific data directory
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html).

## Value

A list with two elements:

- **eem_index**: a `data.frame` of all fluorescence indices. Each row
  corresponds to a single index for a sample.

- **abs_index**: a `data.frame` of all absorbance indices. Each row
  corresponds to a single index for a sample.

Each `data.frame` contains the following columns:

- **sample_name**: name of the sample from the EEM or absorbance list

- **meta_name**: sample name from metadata if provided; otherwise same
  as `sample_name`

- **index**: name of the index

- **value**: calculated value of the index

## Details

**Absorbance indices** (a254, a300, E2_E3, E4_E6, S275_295, S350_400,
S300_700, SR) are calculated using
[`staRdom::abs_parms()`](https://rdrr.io/pkg/staRdom/man/abs_parms.html).

**Fluorescence indices** are calculated using the following
[eemR::eemR](https://rdrr.io/pkg/eemR/man/eemR-package.html) functions:

- Coble peaks (b, t, a, m, c):
  [`eemR::eem_coble_peaks()`](https://rdrr.io/pkg/eemR/man/eem_coble_peaks.html)

- fi (fluorescence index):
  [`eemR::eem_fluorescence_index()`](https://rdrr.io/pkg/eemR/man/eem_fluorescence_index.html)

- hix (humification index):
  [`eemR::eem_humification_index()`](https://rdrr.io/pkg/eemR/man/eem_humification_index.html)
  (use `scale = TRUE` for `hix_scaled`)

- bix (biological index):
  [`eemR::eem_biological_index()`](https://rdrr.io/pkg/eemR/man/eem_biological_index.html)

## Examples

``` r
indices <- eemR_indices(
  eemlist = example_processed_eems,
  abslist = example_processed_abs,
  qaqc_dir = system.file("extdata", package = "eemanalyzeR")
)

# View fluorescence indices
head(indices$eem_index)
#>                           sample_name        meta_name index             value
#> 1                 B1S1ExampleBlankSEM     ExampleBlank     b            DATA01
#> 2                B1S2ExampleTeaStdSEM    ExampleTeaStd     b            DATA01
#> 3                B1S3ExampleSampleSEM    ExampleSample     b            DATA01
#> 4 ManualExampleTeaWaterfallPlotSample ManualExampleTea     b            DATA01
#> 5                 B1S1ExampleBlankSEM     ExampleBlank     t             MDL01
#> 6                B1S2ExampleTeaStdSEM    ExampleTeaStd     t 0.589498543201255

# View absorbance indices
head(indices$abs_index)
#>                        sample_name        meta_name index            value
#> 1              B1S1ExampleBlankABS     ExampleBlank  a254            MDL01
#> 2             B1S2ExampleTeaStdABS    ExampleTeaStd  a254 37.5817130707345
#> 3             B1S3ExampleSampleABS    ExampleSample  a254 18.5704030241273
#> 4 ManualExampleTeaAbsSpectraGraphs ManualExampleTea  a254 69.2896785319941
#> 5              B1S1ExampleBlankABS     ExampleBlank  a300            MDL01
#> 6             B1S2ExampleTeaStdABS    ExampleTeaStd  a300 20.2099169663529
```
