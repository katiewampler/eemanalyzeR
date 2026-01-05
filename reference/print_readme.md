# Print \`readme\`

Nicely print the processing documentation readme file with proper
formatting. The readme tracks the processing steps applied to the
dataset and is stored as an object called \`readme\` in the package
environment.

## Usage

``` r
print_readme()
```

## Value

None (invisible NULL).

## Examples

``` r
print_readme()
#> 2026-01-05 18:36
#> Data processed using eemanalyzeR 1.0.0 package in R.
#> For details on processing steps, indices, and QA/QC flags see the package website: https://katiewampler.github.io/eemanalyzeR/articles/output-documentation.html
#> ______________________________
#> 
#> 2026-01-05 18:36:15: data was corrected for inner filter effects via 'ife_correct' function
#>    function parameters:
#>   cuvle: 1
#>    warning: removed the following wavelengths in EEM's to match absorbance data wavelengths
#>  excitation: 
#>  emission: 806.452 - 820.768
#> 
#> 2026-01-05 18:36:14: Absorbance and fluorescence indices were calculated using the 'get_indices' function
#>    function parameters:
#>   index_method: eemanalyzeR
#>   return: wide
#>   cuvle: 1
#>   qaqc_dir: /home/runner/work/_temp/Library/eemanalyzeR/extdata
#> 
#> 2026-01-05 18:36:13: Fluorescence indices were checked against method detection limits (MDL)
#> Absorbance indices were checked against method detection limits (MDL)
#> 
#> 2026-01-05 18:36:14: 0% (n=8) of the absorbance indices were greater than 20% of the long-term check standard
#> 27% (n=22) of the fluorescence indices were greater than 20% of the long-term check standard
#> 
```
