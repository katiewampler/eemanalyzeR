# eemR and staRdom methods for fluorescence and absorbance indices

Calculates commonly used absorbance and fluorescence optical indices
from `eemlist` and `abslist` objects using functions from the
[eemR](https://CRAN.R-project.org/package=eemR) and
[staRdom](https://CRAN.R-project.org/package=staRdom) packages. Can
incorporate sample metadata if provided.

## Usage

``` r
eemR_indices(eemlist, abslist, cuvle = 1, qaqc_dir = NA)
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
  Default is NA, which results in NO QAQC CHECKS.

## Value

A list with two elements:

- **eem_index**: a `data.frame` of all fluorescence indices. Each row
  corresponds to a single index for a sample.

- **abs_index**: a `data.frame` of all absorbance indices. Each row
  corresponds to a single index for a sample.

Each `data.frame` contains the following columns:

- **sample_id**: sample identifier for the sample (if provided,
  otherwise uses sample: the sample's file name)

- **sample_name**: sample name or description (if provided, otherwise
  uses sample: the sample's file name)

- **index**: name of the index

- **value**: calculated value of the index

## Details

**Absorbance indices** (a254, a300, E2_E3, E4_E6, S275_295, S350_400,
S300_700, SR) are calculated using
[`staRdom::abs_parms()`](https://rdrr.io/pkg/staRdom/man/abs_parms.html).

**Fluorescence indices** are calculated using the following `eemR`
functions:

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
#>          sample_id    sample_name index             value
#> 1     ExampleBlank Sample Blank 1     b            DATA01
#> 2    ExampleTeaStd  PreTea 1% SRM     b            DATA01
#> 3    ExampleSample Example Sample     b            DATA01
#> 4 ManualExampleTea     1% SRM tea     b            DATA01
#> 5     ExampleBlank Sample Blank 1     t             MDL01
#> 6    ExampleTeaStd  PreTea 1% SRM     t 0.589498543201252

# View absorbance indices
head(indices$abs_index)
#>          sample_id    sample_name index            value
#> 1     ExampleBlank Sample Blank 1  a254            MDL01
#> 2    ExampleTeaStd  PreTea 1% SRM  a254 37.5817130707345
#> 3    ExampleSample Example Sample  a254 18.5704030241273
#> 4 ManualExampleTea     1% SRM tea  a254 69.2896785319941
#> 5     ExampleBlank Sample Blank 1  a300            MDL01
#> 6    ExampleTeaStd  PreTea 1% SRM  a300 20.2099169663529
```
