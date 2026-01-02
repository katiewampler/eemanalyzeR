# Export processed absorbance and fluorescence data

Exports processed EEM and absorbance data, plots, indices, and metadata
to a specified output directory. Creates a folder named by `filename`
and saves all specified outputs there.

## Usage

``` r
export_data(
  eemlist,
  abslist,
  filename,
  meta = NULL,
  indices = NULL,
  eem_plot = NULL,
  abs_plot = NULL,
  csv = FALSE,
  output_dir = NA
)
```

## Arguments

- eemlist:

  An `eemlist` object.

- abslist:

  An `abslist` object.

- filename:

  A character string, used for file names.

- meta:

  A `data.frame` containing sample metadata.

- indices:

  If `NULL`, indices will not be exported. If a list of indices is
  provided, it is saved.

- eem_plot:

  If `NULL`, EEM plots will not be exported. If a list of plots is
  provided, they are saved as `.png` files. See
  [`plot.eemlist()`](https://katiewampler.github.io/eemanalyzeR/reference/plot.md)
  for plotting.

- abs_plot:

  If `NULL`, absorbance plots will not be exported. If a plot is
  provided, it is saved as a `.png` file. See
  [`plot.abslist()`](https://katiewampler.github.io/eemanalyzeR/reference/plot.md)
  for plotting.

- csv:

  Logical. If `TRUE`, processed EEM and absorbance data and metadata are
  written to `output_dir` as `.csv` files.

- output_dir:

  Path to save the data. Defaults to a temporary directory if not
  specified.

## Value

A list of the processed data which is also saved to the output directory
specified. The list contains:

- **eemlist:** the `eemlist`

- **abslist:** the `abslist`

- **readme:** a character vector containing information about the data
  processing steps

- **meta:** the metadata associated with the samples, may be `NULL` if
  not provided

- **indices:** a list of EEMs and absorbance indices, may be `NULL` if
  not provided

- **eem_plot:** a list of EEMs plots, may be `NULL` if not provided

- **abs_plot:** a ggplot2 object of the absorbance data, may be `NULL`
  if not provided

## Examples

``` r
eem_plots <- plot(example_processed_eems)
abs_plot <- plot(example_processed_abs)

indices <- get_indices(example_processed_eems, example_processed_abs)
#> Warning: fluorescence MDL is missing, indices will not be checked for MDLs
#> Warning: absorbance MDL is missing, indices will not be checked for MDLs
#> Warning: tea check standard files are missing, check standards will not be checked against the long-term standard
data <- export_data(
  eemlist = example_processed_eems,
  abslist = example_processed_abs,
  filename = "eemanalyzeR_example",
  indices = indices,
  eem_plot = eem_plots,
  abs_plot = abs_plot,
  meta = metadata
)
#> Data successfully exported to: /tmp/RtmpxiFmjg/eemanalyzeR_example
```
