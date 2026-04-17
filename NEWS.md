# Changelog

## eemanalyzeR 1.2.0 (2026-04-16)

-   Moving some of the checks of the QA/QC samples to the `run_eems`
    function so they don't warn the user multiple times as the
    `check_mdl` and `check_std` are run several times throughout
    `run_eems`.

-   Updated `validate_blanks`

    -   to clip to the area of interest before plotting so it is easier
        to see any issues

    -   increased the width of the scattering to remove a little to
        better see any issues

    -   added a message directing users to check out the
        `output-documentation` for checking the blank which prints once
        per session.

-   **Updating vignettes.**

    -   Added `custom-process-steps` to describe how to update and save
        changes to defaults and use multiple methods.

    -   Added figures and snippets of the output files in
        `output-documentation` and `getting-started`.

    -   Added section in `output-documentation` to describe what to look
        for in the blank validation step. (#19)

## eemanalyzeR 1.1.0 (2026-03-31)

-   Updated `create_mdl` and `create_std` functions to ask to update the
    default qaqc_dir in the user_config if it's NA and not directory is
    provided. This fixes the issue caused by version 1.0.2 which
    de-automated creating and then using the QA/QC standard files (#22).

-   Modified how mdl and std files are saved, adding an argument
    `method` to the functions which is used to give a unique name for
    different QA/QC methods (#24).

-   Created a new helper function `get_qaqc` which will look for QA/QC
    files, ask the user which set to use if there's more than one set,
    update the readme with the files used, and return the QA/QC files
    (#24).

## eemanalyzeR 1.0.3 (2026-03-30)

-   Added an argument (`sum_plot`) to `export_data` to skip the summary
    plot to fix bug when processing a large (\>50) samples at a time
    (#28).

-   Added check when running `check_mdl` or `check_std` to warn if the
    wavelengths differ between the QA/QC file and the samples (#24).

-   Further updated the pattern defaults to be a little more flexible
    for `skip` in `dir_read` to work with our example data (#20).

## eemanalyzeR 1.0.2 (2026-03-26)

-   Updating all functions to remove the use of helper function
    `.qaqc_dir` and replace with `get_qaqc_dir` which pulls from the
    config file (#22).

-   Updated the default value for `qaqc_dir` in the config file to
    `.na.character`.

-   When creating MDL and check standard files, if no `qaqc_dir` is
    specified it will return the file as an R object with a warning.

## eemanalyzeR 1.0.1 (2026-03-09)

-   Updated defaults for `skip` in `dir_read` to be more robust and to
    skip samples with "abs" in the name (#20).

## eemanalyzeR 1.0.0 (2026-01-02)

Initial publicly available version.
