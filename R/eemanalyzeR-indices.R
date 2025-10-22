#TODO: add in readme if mdl isn't checked

#' Default package methods for fluorescence and absorbance indices
#'
#' Calculates commonly used absorbance and fluorescence optical indices from eemlist and abslist.
#' For detailed descriptions and references for indices, see the vignette
#' \href{../doc/eemanalyzeR-indices.html}{\code{eemanalyzeR-indices}}.
#' @importFrom zoo na.approx
#' @importFrom tidyr pivot_wider
#' @importFrom pracma interp2
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param cuvle cuvette (path) length in cm
#' @param mdl_dir file path to the mdl files generated with \link[eemanalyzeR]{get_mdl},
#' default is a user-specific data directory (\link[rappdirs]{user_data_dir})
#'
#' @note If absorbance is not at a 1 nanometer interval, absorbance will be interpolated using
#' \link[zoo]{na.approx} which fills in missing values
#' using linear interpolation.
#'
#' If EEMs data is not a 1 nanometer interval, fluorescence will be interpolated
#' using \link[pracma]{interp2}.
#'
#' @return a list with two objects:
#' \itemize{
#'   \item eem_index: a data.frame of all the fluorescence indices
#'   \item abs_index: a data.frame of all the absorbance indices
#' }
#' Each data.frame will have four columns:
#' \itemize{
#'  \item sample_name: the name of the sample
#'  \item meta_name: the name of the sample in the metadata if metadata has been added, otherwise the sample name again
#'  \item index: the name of the index being reported, see \href{../doc/eemanalyzeR-indices.html}{vignette}for more information.
#'  \item value: the value of the index
#' }
#'
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, example_absorbance)
#' eemlist <- add_metadata(metadata, example_eems)
#' eemlist <- add_blanks(eemlist, validate=FALSE)
#' eemlist <- process_eem(eemlist, abslist)
#' indices <- eemanalyzeR_indices(eemlist, abslist,
#' mdl_dir = system.file("extdata", package = "eemanalyzeR"))
eemanalyzeR_indices <- function(eemlist, abslist, cuvle=1, mdl_dir=.qaqc_dir()){
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist), is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized"))==FALSE)

 #get mdl data
  check_eem <- file.exists(file.path(mdl_dir, "eem-mdl.rds"))
  check_abs <- file.exists(file.path(mdl_dir, "abs-mdl.rds"))

  #load mdl data or warn
  if(!check_eem){
    warning("fluoresence MDL is missing, indices will not be checked for MDLs")
    }else{eem_mdl <- readRDS(file.path(mdl_dir, "eem-mdl.rds"))}

  if(!check_abs){
    warning("absorbance MDL is missing, indices will not be checked for MDLs")
    }else{abs_mdl <- readRDS(file.path(mdl_dir, "abs-mdl.rds"))}


 #fluorescence indices
  #define wavelengths for peaks and metrics to check if there are missing wavelengths
    #format: index = list(excitation wavelengths, emission wavelengths)
  peaks <- list(pB = list(ex=270:280, em=300:320),
                pT = list(ex=270:280, em=320:350),
                pA = list(ex=250:260, em=380:480),
                pM = list(ex=310:320, em=380:420),
                pC = list(ex=330:350, em=420:480),
                pD = list(ex=390, em=509),
                pE = list(ex=455, em=521),
                pN = list(ex=280, em=370),
                rAT = list(ex=c(250:260,270:280), em=c(380:480,320:350)),
                rCA = list(ex=c(250:260,330:350), em=c(380:480,420:480)),
                rCM = list(ex=c(330:350,310:320), em=c(420:480,380:420)),
                rCT = list(ex=c(330:350,270:280), em=c(420:480,320:350)),
                pB_DOCnorm = list(ex=270:280, em=300:320),
                pT_DOCnorm = list(ex=270:280, em=320:350),
                pA_DOCnorm = list(ex=250:260, em=380:480),
                pM_DOCnorm = list(ex=310:320, em=380:420),
                pC_DOCnorm = list(ex=330:350, em=420:480),
                pD_DOCnorm = list(ex=390, em=509),
                pE_DOCnorm = list(ex=455, em=521),
                pN_DOCnorm = list(ex=280, em=370),
                FI = list(ex=370, em=c(470, 520)),
                HIX = list(ex=254, em=c(300:345,435:480)),
                HIX_ohno = list(ex=254, em=c(300:345,435:480)),
                fresh = list(ex=310, em=c(380, 420:435)),
                BIX = list(ex=310, em=c(380, 430)))

    #get coble peaks and ratios
      coble <- lapply(names(peaks[1:8]), function(index_name){
        index <- peaks[[index_name]]

        #get values
        vals <- get_fluorescence(eemlist, index$ex, index$em, stat = "max")

        #get flags
          missflags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=FALSE)
          mdlflags <- check_eem_mdl(eemlist, eem_mdl, index$ex, index$em)
          flags <- .combine_flags(missflags, mdlflags)

        #add sample names and make into data.frame (get index name)
        res <- format_index(eemlist, index_name, vals, flags)

        #return res
        return(res)
      }) %>% dplyr::bind_rows()

      ratio_map <- list(rAT = c("pA", "pT"),
                        rCA = c("pC", "pA"),
                        rCM = c("pC", "pM"),
                        rCT = c("pC", "pT"))
      coble_ratios <- lapply(names(peaks[9:12]), function(index_name){
        index <- peaks[[index_name]]

        #get values
        ratio_keys <- ratio_map[[index_name]]
        numerator <- coble$value[coble$index == ratio_keys[1]]
        denominator <- coble$value[coble$index == ratio_keys[2]]

        #remove flags if needed
        numerator <- stringr::str_split_i(numerator, "_", i=1)
        denominator <- stringr::str_split_i(denominator, "_", i=1)

        #if flag is a DATA01, will get "DATA" replace with NA
        numerator[numerator %in% c("DATA01", "NOISE01", "DOC01", "DATA02", "DATA03", "MDL01")] <- NA
        denominator[denominator %in% c("DATA01", "NOISE01", "DOC01", "DATA02", "DATA03", "MDL01")] <- NA

        #get values
        vals <- get_ratios(numerator, denominator)

        #get flags
        flags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=FALSE)

        #add sample names and make into data.frame (get index name)
        res <- format_index(eemlist, index_name, vals, flags)

        #return res
        return(res)
      }) %>% dplyr::bind_rows()

      coble_norm <- lapply(names(peaks[13:20]), function(index_name){
        index <- peaks[[index_name]]

        #get values
        vals <- get_fluorescence(eemlist, index$ex, index$em, stat = "max", norm=TRUE)

        #get flags
        flags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=FALSE)

        #add sample names and make into data.frame (get index name)
        res <- format_index(eemlist, index_name, vals, flags)

        #return res
        return(res)
      })%>% dplyr::bind_rows()

    #get FI
      index <- "FI"
      vals <- get_ratios(get_fluorescence(eemlist, 370, 470), get_fluorescence(eemlist, 370, 520))
      missflags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, 370, 470), check_eem_mdl(eemlist, eem_mdl, 370, 520))
      flags <- .combine_flags(missflags, mdlflags)
      FI <- format_index(eemlist, index, vals, flags)

    #get HIX
      index <- "HIX"
      low <- get_fluorescence(eemlist, 254, 300:345, stat="sum")
      high <- get_fluorescence(eemlist, 254, 435:480, stat="sum")
      vals <- get_ratios(high, low)
      missflags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, 254, 300:345), check_eem_mdl(eemlist, eem_mdl, 254, 435:480))
      flags <- .combine_flags(missflags, mdlflags)
      HIX <- format_index(eemlist, index, vals, flags)

      index <- "HIX_ohno"
      low_high <- get_fluorescence(eemlist, 254, c(300:345, 435:480), stat="sum")
      vals <- get_ratios(high, low_high)
      HIX_ohno <- format_index(eemlist, index, vals, flags)

    #get freshness index
      index <- "fresh"
      vals <- get_ratios(get_fluorescence(eemlist, 310, 380), get_fluorescence(eemlist, 310, 420:435, stat="max"))
      missflags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, 310, 380), check_eem_mdl(eemlist, eem_mdl, 310, 420:435))
      flags <- .combine_flags(missflags, mdlflags)
      fresh <- format_index(eemlist, index, vals, flags)

    #get BIX
      index <- "BIX"
      vals <- get_ratios(get_fluorescence(eemlist, 310, 380), get_fluorescence(eemlist, 310, 430))
      missflags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, 310, 380), check_eem_mdl(eemlist, eem_mdl, 310, 430))
      flags <- .combine_flags(missflags, mdlflags)
      BIX <- format_index(eemlist, index, vals, flags)


  #merge indices together
     eem_index <- do.call(rbind, list(coble, coble_ratios, coble_norm, FI, HIX, HIX_ohno, fresh, BIX ))

 #absorbance indices
   #specify wavelengths for flagging missing data
     abs_wl <- list(SUVA254 = 254,
                    SUVA280=280,
                    SVA412 = 412,
                    S275_295 = 275:295,
                    S350_400=350:400,
                    SR = c(275:295, 350:400),
                    E2_E3=c(250,365),
                    E4_E6=c(465,665))

   #get values at specific wavelengths
     suva <- lapply(names(abs_wl[1:3]), function(index_name){
       index <- abs_wl[[index_name]]

       #get values
       vals <- get_absorbance(abslist, wl=index, suva = TRUE)

       #get flags
       flags <- flag_missing(abslist, wl=index)

       #add sample names and make into data.frame (get index name)
       res <- format_index(abslist, index_name, vals, flags)

       #return res
       return(res)
     })
     suva <- do.call(rbind, suva)


   #get spectral slopes
     index <- "S275_295"
     vals <- get_abs_slope(abslist, c(275,295))
     flags <- flag_missing(abslist, wl=abs_wl[[index]])
     S275_295 <- format_index(abslist, index, vals, flags)

     index <- "S350_400"
     vals <- get_abs_slope(abslist, c(350,400))
     flags <- flag_missing(abslist, wl=abs_wl[[index]])
     S350_400 <- format_index(abslist, index, vals, flags)

    #get ratios
       #remove flags if needed
       numerator <- stringr::str_split_i(S275_295$value, "_", i=1)
       denominator <- stringr::str_split_i(S350_400$value, "_", i=1)

       #if flag is a DATA04, will get "DATA" replace with NA
       numerator[numerator == "DATA"] <- NA
       denominator[denominator == "DATA"] <- NA

       index <- "SR"
       vals <- get_ratios(numerator, denominator)
       flags <- flag_missing(abslist, wl=abs_wl[[index]])
       SR <- format_index(abslist, index, vals, flags)

       index <- "E2_E3"
       vals <- get_ratios(get_absorbance(abslist, 250), get_absorbance(abslist, 365))
       flags <- flag_missing(abslist, wl=abs_wl[[index]])
       E2_E3 <- format_index(abslist, index, vals, flags)

       index <- "E4_E6"
       vals <- get_ratios(get_absorbance(abslist, 465), get_absorbance(abslist, 665))
       flags <- flag_missing(abslist, wl=abs_wl[[index]])
       E4_E6 <- format_index(abslist, index, vals, flags)


      abs_index <- do.call(rbind, list(suva, S275_295, S350_400, SR, E2_E3, E4_E6))


  #return indices
  index <- list(abs_index=abs_index, eem_index = eem_index)
  return(index)

}
