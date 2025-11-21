#' Plot excitation emission matrices with ggplot2
#'
#' Used to make a nice plot of one or multiple excitation emission matrices (EEMs) using ggplot2
#'
#' @param x an \code{eem} or \code{eemlist} object containing EEM's data
#' @param nbin the number of bins used in the contour plot
#' @param equal_scale logical, should the scale be the same for all the plots in the eemlist?
#' @param pal the colors used for the fill scale, if not specified it will use the \link[pals]{parula} palette.
#' If less colors are provided than required, it will use \link[grDevices]{colorRampPalette} to fill in colors.
#' @param remove_lower logical, should the area below the first order rayleigh line be set to NA values? This is helpful if there are artifacts in
#' this region that are affecting the color scale.
#' @param annotate logical, should plots show the index regions?
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#' @param ... extra arguments for `plot`


#' @return if eem is an \code{eem} it will return a single ggplot2 object. If eem is an \code{eemlist}, it will return a list of ggplot2 objects.
#' @method plot eem
#' @rdname plot-eem

#' @importFrom ggplot2 labs ggplot aes geom_contour_filled coord_cartesian geom_contour guides element_text scale_fill_manual theme
#' @importFrom ggpubr ggarrange
#' @importFrom pals parula
#' @importFrom staRdom eem_matmult
#' @export
#' @seealso \href{https://ggplot2.tidyverse.org}{ggplot2}
#' @details
#' If you're plotting a single EEM, the object returned is a \code{ggplot2} object, so it is compatible with other ggplot2 functions to
#' further modify a plot as desired. If you're plotting multiple EEMs, the object returned is a list of \code{ggplot2} objects, which can also be
#' modified by specifying which plot you want to modify. See examples for more details.
#'
#' @note
#' If you find that plotting is taking more than a few seconds, it could be due to your default graphics device. See \href{https://forum.posit.co/t/graphics-not-working-in-rstudio/149111}{this} link for information on how to change this.
#' @examples
#'
#' eems <- example_processed_eems
#'
#' #plot just one EEM
#' plot(eems[[3]])
#'
#' #plot all EEMs in an eemlist
#' plots <- plot(eems)
#'
#' #change color scale
#' plot(eems, pal=c("darkblue", "lightblue"))
#'
#' #make color bar consistent across all plots
#' plot(eems, equal_scale=TRUE)
#'
#' #customize using ggplot2 commands
#' plot(eems[[2]]) + ggplot2::labs(title="Test EEM")
#' plot(eems)[[3]] + ggplot2::labs(title="Test EEM")
#'
#' #modify then arrange together
#' plots <- plot(eems)
#' plots[[3]] <- plots[[3]] + ggplot2::labs(title="Test EEM")
#' print(ggpubr::ggarrange(plotlist = plots))
#'
#'#remove lower area below rayleigh line
#' plots <- plot(eems, remove_lower=TRUE)
#'
#' #annotate the plot with the peaks
#' plot(eems[[3]], annotate=TRUE)


plot.eem <- function(x, nbin=8, equal_scale=FALSE, pal=NULL, remove_lower = FALSE,
                     annotate=FALSE, index_method="eemanalyzeR", ...){
  stopifnot(.is_eem(x))

  plot <- .plot_eem(x, nbin=nbin, z_min=NULL, z_max=NULL, pal=pal, lower=remove_lower, annotate = annotate, index_method=index_method)
  return(plot)

}

#' @method plot eemlist
#' @export
#' @rdname plot-eem
plot.eemlist <- function(x, nbin=8, equal_scale=FALSE, pal=NULL, remove_lower = FALSE,
                         annotate=FALSE, index_method="eemanalyzeR", ...){
  stopifnot(.is_eemlist(x))

    #if equal_scale == TRUE, get limits
    if(equal_scale){
      z_max <- max(sapply(x, function(x){max(x$x, na.rm=T)}))
      z_min <- min(sapply(x, function(x){min(x$x, na.rm=T)}))
      scale <- TRUE
    }else{
      z_max <- NULL
      z_min <- NULL
      scale <- FALSE
    }

    if(annotate){
      plot <- lapply(x, .plot_eem, nbin, z_min, z_max, pal, remove_lower, title=TRUE, annotate=TRUE, index_method=index_method)
    }else{plot <- lapply(x, .plot_eem, nbin, z_min, z_max, pal, remove_lower, title=TRUE)}
    names(plot) <- get_sample_info(x, "sample")

    #only show if interactive to prevent a pdf from being written
    if(is_interactive()){
      print(ggpubr::ggarrange(plotlist = plot, common.legend=scale, legend = "right"))
    }else{
      ggpubr::ggarrange(plotlist = plot, common.legend=scale, legend = "right")
    }

    return(invisible(plot))


}

#' Plot an EEMs nicely with ggplot2
#'
#' @param eem an \code{eem} or object containing EEM's data.
#' @param nbin the number of bins used in the contour plot
#' @param z_min the minimum intensity value to plot, if NULL uses the maximum value from the EEM
#' @param z_max the maximum intensity value to plot, if NULL uses the minimum value from the EEM
#' @param pal the colors used for the fill scale, if not specified it will use the \link[pals]{parula} palette
#' @param title logical, if TRUE will inlude the sample name on the plot, if FALSE it will not. Tries to use meta_name, but will use sample if
#' meta_name doesn't exist.
#' @param annotate logical, should plots show the index regions?
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#' @return a ggplot2 object
#' @noRd
#' @seealso \link[ggplot2]
#'
.plot_eem <- function(eem, nbin, z_min, z_max, pal, lower, title=FALSE, annotate=FALSE,
                      index_method="eemanalyzeR"){

  #remove lower region
  if(lower){
      mat <- matrix(data = 1, nrow = nrow(eem$x),
                  ncol = ncol(eem$x))
      ex_mat <- matrix(eem$ex, nrow = nrow(eem$x),
                       ncol = ncol(eem$x), byrow = TRUE)
      em_mat <- matrix(eem$em, nrow = nrow(eem$x),
                       ncol = ncol(eem$x), byrow = FALSE)
      mat <- mat * (ex_mat < em_mat)
      mat[mat == 0] <- NA
      eem$x <- eem$x * mat
  }

  #functions to make nice breaks
  dec_place <- function(x) {floor(log10(abs(x)))}

  #make breaks
    if(all(is.null(c(z_min, z_max)))){
      z_min <- min(eem$x, na.rm=T)
      z_max <- max(eem$x, na.rm=T)
    }

    z_max_nice <- plyr::round_any(z_max, 10^(dec_place(z_max)-1), f=ceiling)
    z_min_nice <- plyr::round_any(z_min, 10^(dec_place(z_min)-1), f=floor)

    #deal with zero values
    if(z_min == 0){z_min_nice <- 0}

    #make labels
    labs <- data.frame(start=signif(seq(z_min_nice,z_max_nice, length.out = nbin+1), 2))
    labs$end <- c(labs$start[2:nrow(labs)], NA)
    labs$label <- paste(as.character(labs$start), as.character(labs$end), sep=" - ")
    labs$label[nrow(labs)] <- paste(labs$start[nrow(labs)], "+", sep="")
    breaks <- as.numeric(labs$start)

    #get colors
    if(is.null(pal)){
      pal <- pals::parula(n=nbin)
    }

    #fill in missing colors if needed
    if(length(pal) < nbin){
      pal <- grDevices::colorRampPalette(pal)(nbin)
    }

    #get label for fill
    if(attr(eem, "is_raman_normalized") & attr(eem, "is_doc_normalized")){
      fill_lab <- "Intensity (R.U. L mgC \U207B\U00B9)"
    } else if(attr(eem, "is_doc_normalized")){
      fill_lab <- "Intensity (L mgC \U207B\U00B9)"
    } else if(attr(eem, "is_raman_normalized")){
      fill_lab <- "Intensity (R.U.)"
    } else{
      fill_lab <- "Raw Intensity"
    }

    #turn into a dataframe with x,y,z
    df_plot <- data.frame(x=rep(eem$ex, each=length(eem$em)),
                          y=rep(eem$em, times=length(eem$ex)), z=as.vector(eem$x)) %>% na.omit()

    #create plot
    plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x=.data$x,y=.data$y,z=.data$z)) +
      ggplot2::geom_contour_filled(breaks=breaks, show.legend = TRUE) +
      ggplot2::coord_cartesian(expand = FALSE) + ggplot2::geom_contour(color="black", breaks=breaks) +
      ggplot2::labs(x="Excitation (nm)", y="Emission (nm)", fill=fill_lab)  +
      ggplot2::guides(fill = ggplot2::guide_legend(title.position = "right",direction = "vertical",
                                                   title.theme = ggplot2::element_text(angle = 90, colour = "black"),
                                                   barheight = .5, barwidth = .95,
                                                   title.hjust = 0.5,
                                                   reverse=TRUE)) +
      ggplot2::scale_fill_manual(labels=labs$label, values=pal, drop=FALSE)

    if(.meta_added(eem)){plot_name <- eem$meta_name}else{plot_name <- eem$sample}

    if(title){
      plot <- plot + ggplot2::labs(subtitle=plot_name) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size=9))
    }

    if(annotate){
      plot <- annotate_plot(plot, index_method=index_method)
    }
  return(plot)
}

#' Annotate EEMs plot with location of indices
#'
#' Adds labels showing the regions within an EEM that are used to calculate
#' the indices, customized to the specific index method used.
#'
#' @param plot a plot of an `eem`
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#'
#' @returns an ggplot2 object
#' @md
#' @export
#'
#' @examples
#' plot <- plot(example_processed_eems[[3]])
#' annotate_plot(plot)
annotate_plot <- function(plot, index_method="eemanalyzeR"){
  stopifnot(inherits(plot, "ggplot"), index_method %in% c("eemanalyzeR", "eemR", "USGS"))

  #specify the peaks based on the method
    if(index_method == "eemanalyzeR"){
      peaks <- list(pB = list(ex=270:280, em=300:320),
                    pT = list(ex=270:280, em=320:350),
                    pA = list(ex=250:260, em=380:480),
                    pM = list(ex=310:320, em=380:420),
                    pC = list(ex=330:350, em=420:480),
                    pD = list(ex=390, em=509),
                    pE = list(ex=455, em=521),
                    pN = list(ex=280, em=370),
                    FI = list(ex=370, em=c(470, 520)),
                    HIX = list(ex=254, em=c(300:345,435:480)),
                    fresh = list(ex=310, em=c(380, 420:435)),
                    BIX = list(ex=310, em=c(380, 430)))}
    if(index_method =="USGS"){
      peaks <- list(pA_32304 = list(ex=260,em=450),
                    pB_32305 = list(ex=275,em=304),
                    pC_52901 = list(ex=340,em=440),
                    pD_32307 = list(ex=390,em=510),
                    FDOM_52902 = list(ex=370,em=460),
                    pM_32309 = list(ex=300,em=390),
                    pN_32310 = list(ex=280,em=370),
                    pT_32311 = list(ex=275,em=340),
                    FI_32312 = list(ex=370, em=c(470,520)),
                    HIX_32313 = list(ex=254, em=c(300:345, 435:480)))}
    if(index_method =="eemR"){
      peaks <- list(b = list(ex=275, em=310),
                    t = list(ex=275, em=340),
                    a = list(ex=260, em=380:460),
                    m = list(ex=312, em=380:420),
                    c = list(ex=350, em=420:480),
                    fi = list(ex=370, em=c(450, 500)),
                    hix = list(ex=254, em=c(300:345,435:480)),
                    bix = list(ex=310, em=c(380, 430)))}

  #helper function to identify if multiple rects are needed
    split_groups <- function(em){
      # Sort and remove duplicates
      em <- sort(unique(em))
      # Find breaks where the gap > 1
      breaks <- cumsum(c(TRUE, diff(em) != 1))
      # Split based on those breaks
      split(em, breaks)
    }
    specify_points <- function(name, ex, em){
      em <- split_groups(em)
        for(y in 1:length(em)){
          is_point <- ifelse(length(ex) == 1 & length(em[[y]]) == 1, TRUE, FALSE)
          labels <- data.frame(index=name, xmin=min(ex), xmax=max(ex),
                               ymin=min(em[[y]]), ymax=max(em[[y]]),
                               x= mean(ex), y=mean(em[[y]]), point=is_point)

          if(length(em) > 1){labels$index <- paste0(name, "-", y)}
          if(y ==1){labs <- labels}else{labs <- rbind(labs, labels)}
        }

      return(labs)
    }

  #get peaks to plot
    lab_df <- lapply(1:length(peaks), function(x){
        name <- names(peaks)[x]
        index <- peaks[[name]]

        labels <- specify_points(name, index$ex, index$em)
        return(labels)}) %>% dplyr::bind_rows()


  #add to the plot
   plot <- plot + ggplot2::geom_point(data=lab_df[lab_df$point == TRUE,], aes(x=.data$xmin, y=.data$ymin),
                                      inherit.aes = FALSE) +
                  ggplot2::geom_rect(data=lab_df[lab_df$point == FALSE,], aes(xmin=.data$xmin, xmax=.data$xmax,
                                                    ymin=.data$ymin, ymax=.data$ymax),
                                     inherit.aes = FALSE, fill="transparent", color="black") +
                  ggrepel::geom_text_repel(data=lab_df[lab_df$point == FALSE,],
                                           aes(x=.data$x, y=.data$y, label=.data$index),
                                           inherit.aes = FALSE) +
                 ggrepel::geom_text_repel(data=lab_df[lab_df$point == TRUE,],
                                          aes(x=.data$x, y=.data$y, label=.data$index),
                                          inherit.aes = FALSE)

  return(plot)
}
