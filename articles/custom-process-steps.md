# Customizing Processing Steps

## Setting up Processing Defaults

`eemanalyzeR` will process files directly “out of the box”, but specific
research projects may have differing processing needs. Commons defaults
that might be useful to modify are the location of the QA/QC directory,
the scattering widths, the patterns used for identifying the different
sample types.

### When are Different Defaults Needed?

1.  **Processing samples from multiple instruments.** Due to differences
    across instruments, each instrument should have it’s own set of
    QA/QC files (MDL and check standards). This can be accomplished by
    setting a different `qaqc_dir` in the defaults or creating a
    instrument specific `user_config` file (see [The User Configuration
    File](#the-user-configuration-file) below).

2.  **Processing samples varying in matrix or concentrations.** The
    width of the raman and rayleigh scattering lines are sensitive to
    matrix and concentration effects. So the default widths for removing
    the scattering (`width`) may not be sufficient or may be too broad
    and need to be adjusted.

3.  **Collecting samples across different wavelength increments.** Some
    instruments allow you to adjust the increment, pixel size, or the
    spacing between measurements this will alter the excitation/emission
    wavelengths and the signal intensity. When these change, the
    necessitate a new set of QA/QC files. Note that samples are often
    run with different integration times, this alone is not enough to
    require a different set of QA/QC files as integration time is
    normalized across the samples and QA/QC files.

4.  **To match project specific sample naming.** Pattern matching with
    sample names is used to separate EEM’s from absorbance samples and
    identify sample types (blanks, check standards, samples). You may
    wish to update those defaults to better match your naming scheme for
    your project or instrument.

  

### How to Change the Defaults

There are **four** ways to use and modify the processing configuration
defaults:

1.  **Don’t change anything**. The package defaults are used by
    [`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md).
    These use the default arguments for each sub-function within
    [`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md).

2.  **Store custom defaults in `user_config` file.** This is a `.yaml`
    file stored within the package files on your computer. By default it
    matches the package defaults, but can be updated via
    [`edit_user_config()`](https://katiewampler.github.io/eemanalyzeR/reference/user_config.md).
    Once this file has been saved, it will be loaded on package load,
    and the processing defaults specified in the `user_config` file will
    be applied.

3.  **Set temporary defaults for the R session:** More temporary
    defaults can be changed using `modify_config()`. Before using the
    [`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
    function use `modify_config()` to set argument values (i.e.,
    `modify_config(cuvle = 2, eem_skip = "badeem")`). This modifies the
    settings for the R session and will be applied to any data
    processing that occurs until the package is reloaded or the R
    session is restarted. After the package is reloaded the defaults
    revert back to the package defaults or (if they exist) user
    defaults.

4.  **Apply one-time defaults:** For one-time changes in the defaults,
    you can supply arguments to
    [`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
    function itself that modify processing *only* during that run. These
    configuration options will not persist across multiple tries of
    [`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
    and must be specified each time.

All relevant argument values for processing are reported in the readme
file generated in data export.

  

### The User Configuration File

As mentioned above, the `user_config` file is a way to set custom
processing defaults that persist across runs on your computer. The
config file looks like this:

``` yaml
---
#for details on the definition of each argument see the correspoanding function documentation

  # arguments for reading in absorbance data from abs_dir_read()
  abs_pattern: .na.character #pattern in function
  abs_skip: "\\w*(SEM|BEM).dat$|^.*Waterfall( ?)Plot( ?)(Blank|Sample).dat$" #skip in function
  abs_file_ext: "dat" #file_ext in function
  abs_recurse_read: FALSE #recursive in function

  # arguments for reading in absorbance data from eem_dir_read()
  eem_pattern: .na.character #pattern in function
  eem_skip: "\\w*ABS.dat$|.*Abs( ?)Spectra( ?)Graphs.dat$" #skip in function
  eem_file_ext: "dat" #file_ext in function
  eem_recurse_read: FALSE #recursive in function
  eem_import_func: "aqualog" #import_function in function

  # metadata arguments for reading and loading metadata from meta_read() and add_metadata()
  meta_file:  .na.character # User can specify the filename
  meta_sheet: .na.character # only needed if excel file
  meta_validate: TRUE       # usually we want to validate the metadata
  iblank_pattern: "BEM$|Waterfall ?Plot ?Blank" # Regex to pattern match instrument blanks
  sblank_pattern: "Blank|blank|BLK"             # Regex to pattern match sample blanks
  check_pattern: "Tea|tea"                      # Regex to pattern match check standards

 # arguments for adding blanks to samples
  blank_validate: TRUE

  # processing arguments from process_eems()
  ex_clip:
    - 247
    - 450
  em_clip:
    - 247
    - 600
  type:
    - TRUE #first order raman
    - TRUE #second order raman
    - TRUE #first order rayleigh
    - TRUE #second order rayleigh
  width:
    - 16 #first order raman
    - 3 #second order raman
    - 30 #first order rayleigh
    - 10 #second order rayleigh
  interpolate:
    - TRUE #first order raman
    - TRUE #second order raman
    - FALSE #first order rayleigh
    - FALSE #second order rayleigh
  method: 1
  cores: 1
  cuvle: 1

  # indices arguments from get_indices()
  index_method: "eemanalyzeR"
  tolerance: 0.2
  return: "wide"
  qaqc_dir: .na.character
  qaqc_method: .na.character

  # exporting the data arguments using export_data()
  filename: "eemanalyzeR-output"
  output_dir: .na.character
  csv: FALSE
  sum_plot: TRUE
```

The file lives within the user-specific data directory on your computer
(`rappdirs::user_data_dir("eemanalyzeR")`). To update the defaults
within the file you can use
[`edit_user_config()`](https://katiewampler.github.io/eemanalyzeR/reference/user_config.md)
which will pull up the file in an editor. When you save the edits, it
will load the updated file to your session. After the edits are made,
every time you load the package it will read the defaults from that
file.

If you had multiple `user_config` files, you could specify the one you
want loaded using
[`load_user_config()`](https://katiewampler.github.io/eemanalyzeR/reference/user_config.md)
use the argument `config_path` to specify the file path to the file you
want loaded. See [Using Different Processing
Methods](#using-different-processing-methods) for more details on this.

To restore your `user_config` back to the package defaults, you can use
[`reset_user_config()`](https://katiewampler.github.io/eemanalyzeR/reference/user_config.md).
If there are issues with the `user_config`, you can attempt to repair it
using `repair_user_config`, which will check for invalid or missing
values and replace them with package defaults.

> To prevent fully overwriting any old settings, your last old file will
> be saved in the same location with the extension `.backup` appended to
> the file name.

  

### Using Different Processing Methods

If you’re consistently processing samples using different defaults you
might consider developing unique methods (see [When are Different
Defaults Needed?](#when-are-different-defaults-needed)). The unique
methods consist of:

- **A method specific `user_config` file.** Prior to using
  [`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
  load the method specific config file:

  ``` r

  load_user_config(config_path = "eemanalyzeR-methods/method1_user_config.yaml")
  run_eems("test-dir")
  ```

- **Method specific QA/QC files (MDL and check standard).** These should
  be created using blanks and check standards that were run consistent
  with the method (same instrument, wavelengths, etc.). When generating
  the files using
  [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
  and
  [`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md)
  you can specify a method name using the argument `method`. This will
  be used when naming the files (i.e., `test-method-eem-mdl.rds`).

  ``` r

  eem_mdl <- create_mdl(dir = "long-term-blanks",
                        type = "eem",
                        method = "test-method")
  ```

  If the argument `qaqc_dir` is set to `NA` (the package default), you
  will be prompted to update your `user_config` with the default
  location specific to your computer,
  `file.path(rappdirs::user_data_dir("eemanalyzeR"), "qaqc-stds")`. You
  can also specify a different directory to store your QA/QC files in,
  but you will need to manually update your `user_config`.

  > If there are multiple sets of QA/QC files detected in your
  > `qaqc_dir` when processing, the code will prompt you to select the
  > method you want to use, this will be used for that processing run
  > only.
