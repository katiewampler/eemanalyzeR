# Creating a Custom Function to Calculate Indices

## Introduction

To maximum flexibility in calculating indices, we’ve enabled the
[`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md)
function to accept custom written functions, allowing users to create
their own function and calculate indices that may not be included in
existing methods.

## Function Requirements

1.  **The function needs to have the following arguments:**
    1.  `eemlist`: an eemlist object containing EEM’s data.
    2.  `abslist`: an abslist object containing absorbance data.
    3.  `cuvle`: the cuvette (path) length in cm.
    4.  `qaqc_dir`: the file path to the mdl files generated with
        [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
2.  **The function must return a list with the following items:**
    1.  `abs_index`: the indices calculated from the absorbance data
    2.  `eem_index`: the indices calculated from the EEMs data

`abs_index` and `eem_index` should both be a data.frame with the
following columns:

- `sample_name`: a character, the name of the sample

- `meta_name`: a character, the name of the sample in the metadata if
  metadata has been added, otherwise the sample name again

- `index`: a character, the name of the index being reported

- `value`: the value of the index, also accepts flag values (see details
  below)

## Helper Functions

**The eemanalyzeR package contains a number of functions helpful in
creating custom indices.**

To get the absorbance values at a certain wavelength the
[`get_absorbance()`](https://katiewampler.github.io/eemanalyzeR/reference/get_absorbance.md)
function can be used. This function can additionally provide specific
absorbance (like SUVA 254).

``` r
abslist <- example_processed_abs

#absorbance at 254 nm
a254 <- eemanalyzeR::get_absorbance(abslist, 254) 
a254 
#> [1] "0.00054853647738303" "0.163215306070913"   "0.0806502356009794" 
#> [4] "0.300921250392953"

#specific absorbance at 254 nm
suva254 <- eemanalyzeR::get_absorbance(abslist, 254, suva=TRUE) 
suva254
#> [1] "DOC01"            "DOC01"            "4.15722863922574" "DOC01"
```

A similar function exists for getting fluorescence values from EEMs
data:
[`get_fluorescence()`](https://katiewampler.github.io/eemanalyzeR/reference/get_fluorescence.md).
Here, you can specify a range of excitation and/or emission wavelengths.
If stat is set to `sum` the sum of fluorescence will be returned,
otherwise if `max` is used the maximum fluorescence across that range
will be returned.

``` r

eemlist <- example_processed_eems

#get maximum fluorescence across ranges for Peak A and D
pA <- get_fluorescence(eemlist, ex=250:260, em=380:480, stat="max")
pA
#> [1] "0.0103767473213219" "0.280569719363857"  "0.340345670357128" 
#> [4] "0.721431003125204"
pD <- get_fluorescence(eemlist, ex=390, em=509, stat="max")
pD 
#> [1] "0.000304395508989419" "0.0199829245761789"   "0.0978435434033764"  
#> [4] "0.0372706531424358"

#get sum of fluorescence across range of Peak A
pA_sum <- get_fluorescence(eemlist, ex=250:260, em=380:480, stat="sum")
pA_sum
#> [1] "0.398846395176868" "15.4652173718784"  "29.1296537896885" 
#> [4] "34.0595693438647"
```

Lastly, we have a function for getting spectral slopes:
[`get_abs_slope()`](https://katiewampler.github.io/eemanalyzeR/reference/get_abs_slope.md)
which will convert the absorbance to absorption, interpolate the data if
needed, and return the spectral slope within a wavelength range.

``` r
S275_295 <- get_abs_slope(abslist, lim=c(275,295))
```

## QA/QC Flags

While the
[`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md)
function includes some QA/QC checks (negative numbers, missing values,
below noise, outside expected range, etc.), there are some checks that
are easier to do when calculating the indices, or you may wish to
include custom flags or checks. To pass flags from your index function
to the output, they need to get put in the `value` column of the index
data.frames.

There are a few rules to follow with flags:

1.  Flags should have the form text## (DOC01, DATA04, etc.)

2.  If an index value is NA and you want to flag with a reason why it is
    NA, replace the NA value with that flag value.

3.  If you wish to report a value, but still include a flag, report the
    value in the form: value_flag (0.042_DATA01, 43.1_DOC01, etc.)

### Helper Functions

There are few helper functions within the **eemanalyzeR** package that
will provide some common flags.

First is the
[`get_ratios()`](https://katiewampler.github.io/eemanalyzeR/reference/get_ratios.md)
function. This can be helpful when calculating things like peak ratios,
where a missing value will result in an `NA` or a value of 0 in the
denominator will result in an infinite value.

``` r
#calculate the ratio of Peak A to Peak T
 pA <- get_fluorescence(eemlist, ex=250:260, em=380:480)
 pT <- get_fluorescence(eemlist, ex=270:280, em=320:350)
 rAT <- get_ratios(pA, pT)
 rAT
#> [1] "1.72750162152655"  "0.423565751341601" "4.07129094367473" 
#> [4] "0.450601408727671"
 
  #if Peak T is all 0, will return DATA_03 flag, indicating index couldn't 
    #calculated due to zero in denominator
    rAT <- get_ratios(pA, 0)
    rAT
#> [1] "DATA03" "DATA03" "DATA03" "DATA03"
 
#calculate HIX 
  sum_high <- get_fluorescence(eemlist, ex=254, em=435:480, stat = "sum")
  sum_low <- get_fluorescence(eemlist, ex=254, em=300:345, stat="sum")
  HIX <- get_ratios(sum_high,sum_low)
  HIX
#> [1] "DATA01" "DATA01" "DATA01" "DATA01"
  
  #if sum_low is NA, will return DATA_01 flag indicating the index couldn't be 
    #calculated due to missing data
    HIX <- get_ratios(sum_high, NA)
    HIX
#> [1] "DATA01" "DATA01" "DATA01" "DATA01"
```

There’s also a function that will flag indices that can’t be calculated
because data didn’t exist for the requested wavelengths:
[`flag_missing()`](https://katiewampler.github.io/eemanalyzeR/reference/flag_missing.md).
This is useful to check when calculating indices to flag why certain
indices might be returning `NA` values.

``` r
#checking absorbance data
  #data exists, so there are no flags, NA is returned
  flag_missing(abslist, wl=400) 
#> [1] NA NA NA NA

 #data doesn't exist
  flag_missing(abslist, wl=100)
#> [1] "DATA01" "DATA01" "DATA01" "DATA01"
  
 #some data exists, still calculate
  flag_missing(abslist, wl=100:254, all=FALSE)
#> [1] "DATA02" "DATA02" "DATA02" "DATA02"

#checking fluorescence data
  #data exists, so there are no flags, NA is returned
  flag_missing(eemlist, ex=270:280, em=300:320) 
#> [1] NA NA NA NA
  
  #data doesn't exist
  flag_missing(eemlist, ex=100:150, em=300:320) 
#> [1] "DATA01" "DATA01" "DATA01" "DATA01"
  
  #some data exists, still calculate
  flag_missing(eemlist, ex=100:350, em=300:320, all=FALSE) 
#> [1] "DATA02" "DATA02" "DATA02" "DATA02"
```

You may also want to flag values below the method detection limit (MDL)
this can be done with:
[`check_eem_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/check_mdl.md)
and
[`check_abs_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/check_mdl.md).
This is useful to ensure that the blanks are actually blank and ratio
values that are returned are based on “real” data.

``` r
#checking absorbance data
  #get mdl 
    abs_mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"), 
                                 "abs-mdl.rds"))

  #data is fully above the MDL so NA is returned 
    check_abs_mdl(abslist[[2]], abs_mdl, wl=254)
#> [1] NA
    
  #data is fully below the MDL so MDL01 is returned
    check_abs_mdl(abslist[[1]], abs_mdl, wl=254)
#> [1] "MDL01"
    
#checking fluorescence data
  #get mdl 
    eem_mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"), 
                                 "eem-mdl.rds"))
    
  #data is completely above the MDL so NA is returned
   check_eem_mdl(eemlist[[2]], mdl = eem_mdl, ex=270:280, em=300:320)
#> [1] NA
    
  #data is fully below the MDL so MDL01 is returned
   check_eem_mdl(eemlist[[1]], mdl = eem_mdl, ex=270:280, em=300:320)
#> [1] "MDL01"

  #some of the data is below the MDL so MDL02 is returned
    eemlist[[2]]$x[1,4] <- 0.000001 #set a low value so it spans the MDL in the region for an example
    check_eem_mdl(eemlist[[2]], mdl = eem_mdl, ex = 314, em=250:262)
#> [1] "MDL02"
```

If your function generates more than one flag (say a flag for missing
data and an MDL flag) you can use the
[`.combine_flags()`](https://katiewampler.github.io/eemanalyzeR/reference/dot-combine_flags.md)
function to nicely combine the flags.

``` r
  #works with a single set of flags 
    .combine_flags("DATA01", NA)
#> [1] "DATA01"
    .combine_flags(NA, "MDL01")
#> [1] "MDL01"
    .combine_flags(NA, NA)
#> [1] NA
    .combine_flags("DATAO1", "MDL01")
#> [1] "DATAO1_MDL01"
    .combine_flags("DATA01", "DATA01")
#> [1] "DATA01"
  
  #also works with a vector of flags 
    flag1 <- c("MDL01", "MDL02", NA)
    flag2 <- c("DATA01", NA, NA)
    .combine_flags(flag1, flag2)
#> [1] "MDL01_DATA01" "MDL02"        NA
    
  #if you set MDL to TRUE, it knows it's combining two MDL flags 
    #so if one is MDL01 and one is MDL02, it will return MDL02, some of the 
    #data was below the MDL 
    #this function is useful for indices using ratios
    .combine_flags("MDL01", "MDL02", mdl=TRUE)
#> [1] "MDL03"
```

Once we’ve gotten the values and the flags, the data needs to be
formatted in a specific way to input into the
[`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md)
function. To do this we can use the
[`format_index()`](https://katiewampler.github.io/eemanalyzeR/reference/format_index.md)
function.

``` r
  ex <- 240:260
  em <- 300:320
  vals <- get_fluorescence(eemlist, ex, em, stat = "max")
  flags <- flag_missing(eemlist, ex=ex, em=em, all=FALSE)
  index_formatted <- format_index(eemlist, "test_index", vals, flags)
  index_formatted
#>                           sample_name        meta_name      index
#> 1                 B1S1ExampleBlankSEM     ExampleBlank test_index
#> 2                B1S2ExampleTeaStdSEM    ExampleTeaStd test_index
#> 3                B1S3ExampleSampleSEM    ExampleSample test_index
#> 4 ManualExampleTeaWaterfallPlotSample ManualExampleTea test_index
#>                         value
#> 1 0.000532084537187436_DATA02
#> 2    0.520903992822664_DATA02
#> 3   0.0851942070835534_DATA02
#> 4     1.48625281995149_DATA02
```

## Example 1

Alright, now that we’ve reviewed all the helper functions and the
requirements for the custom function let’s do an example. Let’s create a
custom function to calculate some new fluorescence indices presented in
**Zhang et al. 2025:**

$$\text{U-SoI} = \frac{U1_{ex:245,em:440}}{U2_{ex:230,em:260}}$$

$$\text{A-SoI} = \frac{A1_{ex:245,em:325}}{A2_{ex:260,em:430}}$$

$$\text{T-SoI} = \frac{T1_{ex:260,em:430}}{T2_{ex:285,em:365}}$$

First let’s turn the different indices into a list of excitation and
emission wavelengths.

``` r
zhang_indices <- list(USoI = list(ex=c(245,230), em=c(440,260)),
                     ASoI = list(ex=c(245,260), em=c(325,430)),
                     TSoI = list(ex=c(260,285), em=c(430,365)))
```

We can use the
[`get_fluorescence()`](https://katiewampler.github.io/eemanalyzeR/reference/get_fluorescence.md)
helper function to calculate the fluorescence for the numerator and
denominator of the ratio. Then we can use the
[`get_ratios()`](https://katiewampler.github.io/eemanalyzeR/reference/get_ratios.md)
helper function to get the ratio. For a single index this would look
like:

``` r
eemlist <- example_processed_eems
get_ratios(get_fluorescence(eemlist, zhang_indices$TSoI$ex[1], zhang_indices$TSoI$em[1]), 
           get_fluorescence(eemlist, zhang_indices$TSoI$ex[2], zhang_indices$TSoI$em[2]))
#> [1] "4.93586355290846"  "0.528716173072744" "3.87515734359415" 
#> [4] "0.48620210278511"
```

To get all the indices let’s use the lapply function.

``` r
results <- lapply(names(zhang_indices), function(index_name) {
  index <- zhang_indices[[index_name]]
  get_ratios(get_fluorescence(eemlist, index$ex[1], index$em[1]), 
           get_fluorescence(eemlist, index$ex[2], index$em[2]))
  
  })
```

While we’re calculating each index, let’s also use the
[`flag_missing()`](https://katiewampler.github.io/eemanalyzeR/reference/flag_missing.md)
function to see if there’s any data missing. Since we’re finding the
maximum value, we’ll set `all` to `FALSE` so it will report a value if
there’s some data. We’ll also check to see if any of the data is below
the MDL, since we have two discrete values we can use
[`.combine_flags()`](https://katiewampler.github.io/eemanalyzeR/reference/dot-combine_flags.md)
to combine the returned MDL flags. Lastly, we’ll use the
[`format_index()`](https://katiewampler.github.io/eemanalyzeR/reference/format_index.md)
function to make sure everything is correctly formatted.

``` r
results <- lapply(names(zhang_indices), function(index_name){
  #get values
  index <- zhang_indices[[index_name]]
  vals <- get_ratios(get_fluorescence(eemlist, index$ex[1], index$em[1]), 
           get_fluorescence(eemlist, index$ex[2], index$em[2]))

  #get flags
    missflags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=TRUE)
    mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, index$ex[1], index$em[1]), 
                               check_eem_mdl(eemlist, eem_mdl, index$ex[2], index$em[2]), mdl=TRUE)
    flags <- .combine_flags(missflags, mdlflags)

  #add sample names and make into data.frame (get index name)
  res <- format_index(eemlist, index_name, vals, flags)
    
  #return res
  return(res)
  })

results <- do.call(rbind, results)
```

Now we need to make sure to add in the MDL data so we can use it to
check our values. If the MDL is NULL, we want to have a warning, but
still run.

``` r
 qaqc_dir <- file.path(system.file("extdata", package = "eemanalyzeR"))
 #get mdl data
  check_eem <- file.exists(file.path(qaqc_dir, "eem-mdl.rds"))

  #load mdl data or warn
  if(!check_eem){
    warning("fluorescence MDL is missing, indices will not be checked for MDLs")
    eem_mdl <- NULL
    }else{eem_mdl <- readRDS(file.path(qaqc_dir, "eem-mdl.rds"))}
```

Lastly, we just need to return the results as a list. Since we don’t
have any absorbance data we’ll just make it NA.

``` r
  list(abs_index = NA,
       eem_index=results)
#> $abs_index
#> [1] NA
#> 
#> $eem_index
#>                            sample_name        meta_name index             value
#> 1                  B1S1ExampleBlankSEM     ExampleBlank  USoI            DATA01
#> 2                 B1S2ExampleTeaStdSEM    ExampleTeaStd  USoI            DATA01
#> 3                 B1S3ExampleSampleSEM    ExampleSample  USoI            DATA01
#> 4  ManualExampleTeaWaterfallPlotSample ManualExampleTea  USoI            DATA01
#> 5                  B1S1ExampleBlankSEM     ExampleBlank  ASoI      DATA01_MDL03
#> 6                 B1S2ExampleTeaStdSEM    ExampleTeaStd  ASoI            DATA01
#> 7                 B1S3ExampleSampleSEM    ExampleSample  ASoI            DATA01
#> 8  ManualExampleTeaWaterfallPlotSample ManualExampleTea  ASoI            DATA01
#> 9                  B1S1ExampleBlankSEM     ExampleBlank  TSoI             MDL01
#> 10                B1S2ExampleTeaStdSEM    ExampleTeaStd  TSoI 0.528716173072744
#> 11                B1S3ExampleSampleSEM    ExampleSample  TSoI  3.87515734359415
#> 12 ManualExampleTeaWaterfallPlotSample ManualExampleTea  TSoI  0.48620210278511
```

Great, that all looks good, now we just need to combine all that code to
a custom function.

``` r
zhang2025 <- function(eemlist, abslist, cuvle=1, qaqc_dir){
  #making sure eemlist and abslist are correct objects
    stopifnot(eemanalyzeR:::.is_eemlist(eemlist), eemanalyzeR:::.is_abslist(abslist), 
              is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized"))==FALSE)
  
  #get mdl data
  check_eem <- file.exists(file.path(qaqc_dir, "eem-mdl.rds"))

  #load mdl data or warn
  if(!check_eem){
    warning("fluorescence MDL is missing, indices will not be checked for MDLs")
    eem_mdl <- NULL
  }else{eem_mdl <- readRDS(file.path(qaqc_dir, "eem-mdl.rds"))}
  
  #specify indices
    zhang_indices <- list(USoI = list(ex=c(245,230), em=c(440,260)),
                     ASoI = list(ex=c(245,260), em=c(325,430)),
                     TSoI = list(ex=c(260,285), em=c(430,365)))
  
  #get results as a data.frame
   results <- lapply(names(zhang_indices), function(index_name){
      index <- zhang_indices[[index_name]]
      vals <- get_ratios(get_fluorescence(eemlist, index$ex[1], index$em[1]), 
               get_fluorescence(eemlist, index$ex[2], index$em[2]))        
      
      missflags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=TRUE)
      mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, index$ex[1], index$em[1]), 
                                   check_eem_mdl(eemlist, eem_mdl, index$ex[2], index$em[2]), mdl=TRUE)
      flags <- .combine_flags(missflags, mdlflags)
    
      res <- format_index(eemlist, index_name, vals, flags)
        
      return(res)
      })

  #combine lists together
    results <- do.call(rbind, results)

  return(list(abs_index = NA,
         eem_index=results))
}
```

Now let’s test our custom function with the
[`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md)
function. Note that we get a warning about the indices because the EEMs
data hasn’t gone through any processing steps.

``` r
indices <- get_indices(example_processed_eems, 
                       example_processed_abs, 
                       index_method = zhang2025, 
                       return = "wide", 
                       qaqc_dir = file.path(system.file("extdata", package = "eemanalyzeR")))

indices$eem_index
#> # A tibble: 4 × 5
#>   sample_name                         meta_name        USoI   ASoI         TSoI 
#>   <chr>                               <chr>            <chr>  <chr>        <chr>
#> 1 B1S1ExampleBlankSEM                 ExampleBlank     DATA01 DATA01_MDL03 MDL01
#> 2 B1S2ExampleTeaStdSEM                ExampleTeaStd    DATA01 DATA01       0.52…
#> 3 B1S3ExampleSampleSEM                ExampleSample    DATA01 DATA01       3.875
#> 4 ManualExampleTeaWaterfallPlotSample ManualExampleTea DATA01 DATA01       0.48…
```

## Example 2

The last example only used fluorescence data. Let’s create another
custom function focused on absorbance indices. We’ll create a function
to calculate the indices used in **Erlandsson et al. 2012:**

| Index     | Description                          |
|:----------|:-------------------------------------|
| a254      | absorbance at 254 nm                 |
| a420      | absorbance at 420 nm                 |
| a220_a254 | ratio of absorbance at 220 to 254 nm |
| a250_a364 | ratio of absorbance at 250 to 364 nm |
| a300_a400 | ratio of absorbance at 300 to 400 nm |
| S275_295  | spectral slope between 275 to 295 nm |
| SR        | spectral ratio                       |

First let’s turn the indices into a list of wavelengths to help with
flagging and calculating. Note that for the ratios we only need two
specific wavelengths, so we’re making that a vector of just those two
wavelengths, while the spectral slope needs all the wavelengths between
those two values, so we’re making that a vector of all the wavelengths
between those two wavelengths.

``` r
erlandsson_index <- list(a254=254,
                         a420=420,
                         a220_a254=c(220,250),
                         a254_a364=c(254,364),
                         a300_a400=c(300,400),
                         S275_295=c(275:295),
                         SR=c(275:295, 350:400))
```

We can use the
[`get_absorbance()`](https://katiewampler.github.io/eemanalyzeR/reference/get_absorbance.md)
helper function to calculate the absorbance at 254 and 420. For a single
wavelength that looks like:

``` r
abslist <- example_processed_abs
a254 <- get_absorbance(abslist, wl=erlandsson_index[[1]])
a254
#> [1] "0.00054853647738303" "0.163215306070913"   "0.0806502356009794" 
#> [4] "0.300921250392953"
```

Let’s use lapply to get the absorbance, add any flags, and format it
correctly. We’ll use
[`base::do.call()`](https://rdrr.io/r/base/do.call.html) to bind all the
lists of indices together.

``` r
abs_data <- lapply(names(erlandsson_index[1:2]), function(index_name){
       index <- erlandsson_index[[index_name]]

       #get values
       vals <- get_absorbance(abslist, wl=index) 
       
       #get flags
        missflags <- flag_missing(abslist, wl=index, all=TRUE)
        mdlflags <- check_abs_mdl(abslist, abs_mdl, wl=index)
        flags <- .combine_flags(missflags, mdlflags)

      #add sample names and make into data.frame (get index name)
        res <- format_index(abslist, index_name, vals, flags)

        #return res
        return(res)
     })
abs_data <- do.call(rbind, abs_data)
```

Next lets get the ratios, we can still use the
[`get_absorbance()`](https://katiewampler.github.io/eemanalyzeR/reference/get_absorbance.md)
helper function to get the absorbance at the wavelengths, and then use
the
[`get_ratios()`](https://katiewampler.github.io/eemanalyzeR/reference/get_ratios.md)
helper function to get the ratio with any flags. In the case below, we
don’t have absorbance at 220 nm, so we get flags of DATA01

``` r
a220 <- get_absorbance(abslist, wl=220) 
a254 <- get_absorbance(abslist, wl=254)
a220_a254 <- get_ratios(a220, a254)
a220_a254
#> [1] "DATA01" "DATA01" "DATA01" "DATA01"
```

Since we’re getting multiple different sets of ratios, let’s use
[`base::lapply()`](https://rdrr.io/r/base/lapply.html) to get all of the
ratios at once.

``` r
ratios <- lapply(names(erlandsson_index[3:5]), function(index_name){
       index <- erlandsson_index[[index_name]]

       #get values
       a1 <- get_absorbance(abslist, wl=index[1]) 
       a2 <- get_absorbance(abslist, wl=index[2])
       get_ratios(a1, a2)
     })
```

While we’re calculating each index, let’s also use the
[`flag_missing()`](https://katiewampler.github.io/eemanalyzeR/reference/flag_missing.md)
function to see if there’s any data missing. Lastly, we’ll use the
[`format_index()`](https://katiewampler.github.io/eemanalyzeR/reference/format_index.md)
function to make sure everything is correctly formatted. We’ll use
`do.call` to bind all the lists of indices together.

``` r
ratios <- lapply(names(erlandsson_index[3:5]), function(index_name){
       index <- erlandsson_index[[index_name]]

       #get values
       a1 <- get_absorbance(abslist, wl=index[1]) 
       a2 <- get_absorbance(abslist, wl=index[2])
       vals <- get_ratios(a1, a2)
       
        #get flags
        missflags <- flag_missing(abslist, wl=index, all=TRUE)
        mdlflags <- check_abs_mdl(abslist, abs_mdl, wl=index)
        flags <- .combine_flags(missflags, mdlflags)

      #add sample names and make into data.frame (get index name)
      res <- format_index(abslist, index_name, vals, flags)
        
      #return res
      return(res)
     })
ratios <- do.call(rbind, ratios)
```

Lastly we need to get the spectral slopes and spectral ratio. For these
we’ll use the
[`get_abs_slope()`](https://katiewampler.github.io/eemanalyzeR/reference/get_abs_slope.md)
function.

``` r
index <- "S275_295"
vals <- get_abs_slope(abslist, c(275,295))
missflags <- flag_missing(abslist, wl=erlandsson_index[[index]])
mdlflags <- check_abs_mdl(abslist, mdl=abs_mdl, wl=erlandsson_index[[index]])
flags <- .combine_flags(missflags, mdlflags)
S275_295 <- format_index(abslist, index, vals, flags)

index <- "SR"
vals <- get_ratios(get_abs_slope(abslist, c(275,295)), get_abs_slope(abslist, c(350,400)))
missflags <- flag_missing(abslist, wl=erlandsson_index[[index]])
mdlflags <- .combine_flags(check_abs_mdl(abslist, mdl=abs_mdl, wl=275:295),
                           check_abs_mdl(abslist, mdl=abs_mdl, wl=350:400))
flags <- .combine_flags(missflags, mdlflags)
SR <- format_index(abslist, index, vals, flags)
```

Now we need to make sure to add in the MDL data so we can use it to
check our values. If the MDL is NULL, we want to have a warning, but
still run.

``` r
 qaqc_dir <- file.path(system.file("extdata", package = "eemanalyzeR"))
 #get mdl data
  check_abs <- file.exists(file.path(qaqc_dir, "abs-mdl.rds"))

  #load mdl data or warn
  if(!check_abs){
    warning("absorbance MDL is missing, indices will not be checked for MDLs")
    abs_mdl <- NULL
    }else{abs_mdl <- readRDS(file.path(qaqc_dir, "abs-mdl.rds"))}
```

Lastly, we just need to combine all the indices into a data.frame and
return the results as a list using another
[`base::do.call()`](https://rdrr.io/r/base/do.call.html). Since we don’t
have any EEMs data we’ll just make it NA.

``` r
 results <- do.call(rbind, list(abs_data, ratios, S275_295, SR))

 list(abs_index = results,
       eem_index=NA)
#> $abs_index
#>                         sample_name        meta_name     index
#> 1               B1S1ExampleBlankABS     ExampleBlank      a254
#> 2              B1S2ExampleTeaStdABS    ExampleTeaStd      a254
#> 3              B1S3ExampleSampleABS    ExampleSample      a254
#> 4  ManualExampleTeaAbsSpectraGraphs ManualExampleTea      a254
#> 5               B1S1ExampleBlankABS     ExampleBlank      a420
#> 6              B1S2ExampleTeaStdABS    ExampleTeaStd      a420
#> 7              B1S3ExampleSampleABS    ExampleSample      a420
#> 8  ManualExampleTeaAbsSpectraGraphs ManualExampleTea      a420
#> 9               B1S1ExampleBlankABS     ExampleBlank a220_a254
#> 10             B1S2ExampleTeaStdABS    ExampleTeaStd a220_a254
#> 11             B1S3ExampleSampleABS    ExampleSample a220_a254
#> 12 ManualExampleTeaAbsSpectraGraphs ManualExampleTea a220_a254
#> 13              B1S1ExampleBlankABS     ExampleBlank a254_a364
#> 14             B1S2ExampleTeaStdABS    ExampleTeaStd a254_a364
#> 15             B1S3ExampleSampleABS    ExampleSample a254_a364
#> 16 ManualExampleTeaAbsSpectraGraphs ManualExampleTea a254_a364
#> 17              B1S1ExampleBlankABS     ExampleBlank a300_a400
#> 18             B1S2ExampleTeaStdABS    ExampleTeaStd a300_a400
#> 19             B1S3ExampleSampleABS    ExampleSample a300_a400
#> 20 ManualExampleTeaAbsSpectraGraphs ManualExampleTea a300_a400
#> 21              B1S1ExampleBlankABS     ExampleBlank  S275_295
#> 22             B1S2ExampleTeaStdABS    ExampleTeaStd  S275_295
#> 23             B1S3ExampleSampleABS    ExampleSample  S275_295
#> 24 ManualExampleTeaAbsSpectraGraphs ManualExampleTea  S275_295
#> 25              B1S1ExampleBlankABS     ExampleBlank        SR
#> 26             B1S2ExampleTeaStdABS    ExampleTeaStd        SR
#> 27             B1S3ExampleSampleABS    ExampleSample        SR
#> 28 ManualExampleTeaAbsSpectraGraphs ManualExampleTea        SR
#>                 value
#> 1               MDL01
#> 2   0.163215306070913
#> 3  0.0806502356009794
#> 4   0.300921250392953
#> 5               MDL01
#> 6  0.0097782541629343
#> 7   0.005703926210965
#> 8  0.0174349110598922
#> 9        DATA01_MDL01
#> 10             DATA01
#> 11             DATA01
#> 12             DATA01
#> 13              MDL01
#> 14   4.30029009047272
#> 15   5.03597089422996
#> 16   4.47739151485424
#> 17              MDL01
#> 18   6.02242754070733
#> 19   5.46607410055233
#> 20   6.50764672772594
#> 21              MDL01
#> 22 0.0230618505221762
#> 23 0.0130236674635653
#> 24 0.0230606966213098
#> 25              MDL01
#> 26   1.03003744636482
#> 27  0.740240611049655
#> 28   1.00357361747553
#> 
#> $eem_index
#> [1] NA
```

Great, that all looks good, now we just need to combine all that code to
a custom function.

``` r
erlandsson2012 <- function(eemlist, abslist, cuvle=1, qaqc_dir){
  #making sure eemlist and abslist are correct objects
    stopifnot(eemanalyzeR:::.is_eemlist(eemlist), eemanalyzeR:::.is_abslist(abslist), 
              is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized"))==FALSE)
  
  #get mdl 
  check_abs <- file.exists(file.path(qaqc_dir, "abs-mdl.rds"))

  #load mdl data or warn
  if(!check_abs){
    warning("absorbance MDL is missing, indices will not be checked for MDLs")
    abs_mdl <- NULL
  }else{abs_mdl <- readRDS(file.path(qaqc_dir, "abs-mdl.rds"))}
  
  #specify wavelengths
    erlandsson_index <- list(a254=254,
                         a420=420,
                         a220_a254=c(220,250),
                         a254_a364=c(254,364),
                         a300_a400=c(300,400),
                         S275_295=c(275:295),
                         SR=c(275:295, 350:400))
  
  #get results as a data.frame
    abs_data <- lapply(names(erlandsson_index[1:2]), function(index_name){
       index <- erlandsson_index[[index_name]]

       #get values
       vals <- get_absorbance(abslist, wl=index) 
       
       #get flags
        missflags <- flag_missing(abslist, wl=index, all=TRUE)
        mdlflags <- check_abs_mdl(abslist, abs_mdl, wl=index)
        flags <- .combine_flags(missflags, mdlflags)
        
      #add sample names and make into data.frame (get index name)
        res <- format_index(abslist, index_name, vals, flags)

        #return res
        return(res)
     })
    abs_data <- do.call(rbind, abs_data)
  
    ratios <- lapply(names(erlandsson_index[3:5]), function(index_name){
       index <- erlandsson_index[[index_name]]

       #get values
       a1 <- get_absorbance(abslist, wl=index[1]) 
       a2 <- get_absorbance(abslist, wl=index[2])
       vals <- get_ratios(a1, a2)
       
        #get flags
        missflags <- flag_missing(abslist, wl=index, all=TRUE)
        mdlflags <- check_abs_mdl(abslist, abs_mdl, wl=index)
        flags <- .combine_flags(missflags, mdlflags)

      #add sample names and make into data.frame (get index name)
      res <- format_index(abslist, index_name, vals, flags)
        
      #return res
      return(res)
     })
    ratios <- do.call(rbind, ratios) 
    
    index <- "S275_295"
    vals <- get_abs_slope(abslist, c(275,295))
    missflags <- flag_missing(abslist, wl=erlandsson_index[[index]])
    mdlflags <- check_abs_mdl(abslist, mdl=abs_mdl, wl=erlandsson_index[[index]])
    flags <- .combine_flags(missflags, mdlflags)
    S275_295 <- format_index(abslist, index, vals, flags)
    
    index <- "SR"
    vals <- get_ratios(get_abs_slope(abslist, c(275,295)), 
                       get_abs_slope(abslist, c(350,400)))
    missflags <- flag_missing(abslist, wl=erlandsson_index[[index]])
    mdlflags <- .combine_flags(check_abs_mdl(abslist, mdl=abs_mdl, wl=275:295),
                               check_abs_mdl(abslist, mdl=abs_mdl, wl=350:400))
    flags <- .combine_flags(missflags, mdlflags)
    SR <- format_index(abslist, index, vals, flags)

  #combine lists together
    results <- do.call(rbind, list(abs_data, ratios, S275_295, SR))


  return(list(abs_index = results,
       eem_index=NA))
}
```

Now let’s test our custom function with the
[`get_indices()`](https://katiewampler.github.io/eemanalyzeR/reference/get_indices.md)
function. Once again note that we get a warning about the indices
because the EEMs data hasn’t gone through any processing steps.

``` r
indices <- get_indices(example_processed_eems, example_processed_abs, index_method = erlandsson2012, 
                       qaqc_dir = file.path(system.file("extdata", package = "eemanalyzeR")), return="wide")

indices$abs_index
#> # A tibble: 4 × 9
#>   sample_name meta_name a254  a420  a220_a254 a254_a364 a300_a400 S275_295 SR   
#>   <chr>       <chr>     <chr> <chr> <chr>     <chr>     <chr>     <chr>    <chr>
#> 1 B1S1Exampl… ExampleB… MDL01 MDL01 DATA01_M… MDL01     MDL01     MDL01    MDL01
#> 2 B1S2Exampl… ExampleT… 0.16… 0.00… DATA01    4.3       6.022     0.02306  1.03 
#> 3 B1S3Exampl… ExampleS… 0.08… 0.00… DATA01    5.036     5.466     0.01302  0.74…
#> 4 ManualExam… ManualEx… 0.30… 0.01… DATA01    4.477     6.508     0.02306  1.004
```

## References

Zhang, H., Hou, J., Nie, L., Hao, Y., Gao, H., & Yu, H. (2025).
Developing new spectral indices for identifying DOM sources in Liaohe
River in a large-scale river basin by fluorescence spectroscopy and
random forest model. Process Safety and Environmental Protection, 201,
107553. <doi:10.1016/j.psep.2025.107553>

Erlandsson, M., N. Futter, M., N. Kothawala, D., & J. Köhler, S. (2012).
Variability in spectral absorbance metrics across boreal lake waters.
Journal of Environmental Monitoring, 14(10), 2643-2652.
<doi:10.1039/C2EM30266G>
