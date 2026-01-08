# Extract absorbance at a given wavelength

Returns the absorbance value at a specified wavelength, reported in
cm⁻¹. Optionally calculates specific absorbance (SUVA), reported in m⁻¹.

## Usage

``` r
get_absorbance(abs, wl, cuvle = 1, suva = FALSE)
```

## Arguments

- abs:

  An \`abs\` or \`abslist\` object.

- wl:

  A vector of absorbance wavelengths.

- cuvle:

  Cuvette (path) length in cm.

- suva:

  If \`TRUE\`, returns specific absorbance (SUVA).

## Value

A vector of absorbance (or specific absorbance) values. If a value
cannot be extracted, "DOC01" is returned.

## Details

If metadata is not present, SUVA cannot be calculated and \`NA\` will be
returned. To add metadata, use \[add_metadata()\].

If the wavelength requested is not in the absorbance data, it will be
interpolated first using \[abs_interp()\].

## Examples

``` r
abslist <- add_metadata(metadata, example_abs)

a254 <- get_absorbance(abslist, 254)
suva254 <- get_absorbance(abslist, 254, suva = TRUE)
```
