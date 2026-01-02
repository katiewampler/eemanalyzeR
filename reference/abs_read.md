# Read a single absorbance file into R

Will accept absorbance files run via Aqualog exported using the sample Q
or manually. If the function tries to load an EEM file, it will return a
warning and a `NULL` value for absorbance.

## Usage

``` r
abs_read(file)
```

## Arguments

- file:

  File path to the absorbance file to load.

## Value

An object of class `abs` containing:

- **file**: The filename of the absorbance data

- **sample**: The sample name

- **n**: The number of wavelengths measured

- **data**: A matrix where the first column is wavelength (nm) and the
  second column is absorbance

- **location**: Directory where the absorbance file is stored

## Examples

``` r
abs_files <- list.files(
  system.file("extdata", package = "eemanalyzeR"),
  full.names = TRUE,
  pattern = "ABS|Abs ?Spectra ?Graphs")

abs <- abs_read(abs_files[1])
```
