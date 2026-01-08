# Remove Raman and Rayleigh scattering lines from EEMs

Raman and Rayleigh scattering can interfere with sample fluorescence
signals. Raman scattering is inelastic scattering caused by interaction
with the solvent, shifting to longer wavelengths. Rayleigh scattering is
elastic scattering, where light bounces off molecules without energy
loss, appearing on the 1:1 line and often intense. Second-order Raman
and Rayleigh scattering may also occur due to two-photon interactions
doubling the excitation wavelength.

## Usage

``` r
remove_scattering(
  eemlist,
  type = c(TRUE, TRUE, TRUE, TRUE),
  width = c(16, 3, 30, 10),
  interpolate = c(TRUE, TRUE, FALSE, FALSE),
  method = 1,
  cores = 1,
  arg_names = NULL
)
```

## Arguments

- eemlist:

  An `eemlist` object.

- type:

  Logical vector of length four indicating which scattering lines to
  remove. The order is "raman1", "raman2", "rayleigh1", "rayleigh2". Set
  `TRUE` to remove.

- width:

  Numeric vector of length four specifying the width (nm) of the
  scattering lines to remove. Order is "raman1", "raman2", "rayleigh1",
  "rayleigh2".

- interpolate:

  Logical vector of length four indicating which scattering lines to
  interpolate. Order is "raman1", "raman2", "rayleigh1", "rayleigh2".

- method:

  Numeric 0–4 specifying interpolation method (default = 1). See
  [`eem_interp()`](https://rdrr.io/pkg/staRdom/man/eem_interp.html).

- cores:

  Number of cores for parallel interpolation computation.

- arg_names:

  Optional list of arguments passed from higher-level functions for
  README generation.

## Value

An `eemlist` object with specified scattering lines removed.

## Note

This function is a modified version of
[`staRdom::eem_rem_scat()`](https://rdrr.io/pkg/staRdom/man/eem_rem_scat.html)
from the `staRdom` package, allowing selective interpolation instead of
all-or-nothing.

## References

Gilchrist, J. R., & Reynolds, D. M. (2014). Optical Spectroscopy
Instrumentation Design, Quality Assurance, and Control: Bench-Top
Fluorimetry. In A. Baker, D. M. Reynolds, J. Lead, P. G. Coble, & R. G.
M. Spencer (Eds.), Aquatic Organic Matter Fluorescence (pp. 147-189).
Cambridge: Cambridge University Press.
[doi:10.1017/CBO9781139045452.009](https://doi.org/10.1017/CBO9781139045452.009)

## Examples

``` r
# default settings (remove all, interpolate only raman lines)
eemlist <- remove_scattering(example_eems)
plot(eemlist[[6]])


# only remove only rayleigh lines
eemlist <- remove_scattering(example_eems, type = c(FALSE, FALSE, TRUE, TRUE))
plot(eemlist[[6]])


# change the width of the lines
eemlist <- remove_scattering(example_eems, width = c(16, 3, 100, 40))
plot(eemlist[[6]])
```
