#TODO:add option to add peak annotations on
#' Plot excitation emission matrices with ggplot2
#'
#' Used to make a nice plot of one or multiple excitation emission matrices (EEMs) using ggplot2
#'
#' @param eem an \code{eem} or \code{eemlist} object containing EEM's data
#' @param nbin the number of bins used in the contour plot
#' @param equal_scale logical, should the scale be the same for all the plots in the eemlist?
#' @param pal the colors used for the fill scale, if not specified it will use the \link[pals]{parula} palette.
#' If less colors are provided than required, it will use \link[grDevices]{colorRampPalette} to fill in colors.
#' @param remove_lower logical, should the area below the first order rayleigh line be set to NA values? This is helpful if there are artifacts in
#' this region that are affecting the color scale.
#'
#' @return if eem is an \code{eem} it will return a single ggplot2 object. If eem is an \code{eemlist}, it will return a list of ggplot2 objects.
#'
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
#' plot_eem(eems[[3]])
#'
#' #plot all EEMs in an eemlist
#' plots <- plot_eem(eems)
#'
#' #change color scale
#' plot_eem(eems, pal=c("darkblue", "lightblue"))
#'
#' #make color bar consistent across all plots
#' plot_eem(eems, equal_scale=TRUE)
#'
#' #customize using ggplot2 commands
#' plot_eem(eems[[2]]) + ggplot2::labs(title="Test EEM")
#' plot_eem(eems)[[3]] + ggplot2::labs(title="Test EEM")
#'
#' #modify then arrange together
#' plots <- plot_eem(eems)
#' plots[[3]] <- plots[[3]] + ggplot2::labs(title="Test EEM")
#' print(ggpubr::ggarrange(plotlist = plots))
#'
#'#remove lower area below rayleigh line
#' plots <- plot_eem(eems, remove_lower=TRUE)


plot_eem <- function(eem, nbin=8, equal_scale=FALSE, pal=NULL, remove_lower = FALSE){
  stopifnot(.is_eem(eem) | .is_eemlist(eem))
  #if eemlist, apply plotting function to list
  if(.is_eemlist(eem)){
    #if equal_scale == TRUE, get limits
    if(equal_scale){
      z_max <- max(sapply(eem, function(x){max(x$x, na.rm=T)}))
      z_min <- min(sapply(eem, function(x){min(x$x, na.rm=T)}))
      scale <- TRUE
    }else{
      z_max <- NULL
      z_min <- NULL
      scale <- FALSE
    }


    plot <- lapply(eem, .plot_eem, nbin, z_min, z_max, pal, remove_lower, title=TRUE)
    names(plot) <- get_sample_info(eem, "sample")

    #only show if interactive to prevent a pdf from being written
    if(is_interactive()){
      print(ggpubr::ggarrange(plotlist = plot, common.legend=scale, legend = "right"))
    }else{
      ggpubr::ggarrange(plotlist = plot, common.legend=scale, legend = "right")
    }

    return(invisible(plot))
  }

  plot <- .plot_eem(eem, nbin=nbin, z_min=NULL, z_max=NULL, pal=pal, lower=remove_lower)
  return(plot)

}

#' Plot an EEMs nicely with ggplot2
#'
#' @param eem an \code{eem} object containing EEM's data.
#' @param nbin the number of bins used in the contour plot
#' @param z_min the minimum intensity value to plot, if NULL uses the maximum value from the EEM
#' @param z_max the maximum intensity value to plot, if NULL uses the minimum value from the EEM
#' @param pal the colors used for the fill scale, if not specified it will use the \link[pals]{parula} palette
#' @param title logical, if TRUE will inlude the sample name on the plot, if FALSE it will not. Tries to use meta_name, but will use sample if
#' meta_name doesn't exist.
#' @return a ggplot2 object
#' @noRd
#' @seealso \link[ggplot2]
#'
.plot_eem <- function(eem, nbin, z_min, z_max, pal, lower, title=FALSE){

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
                                                   title.hjust = 0.5, raster = FALSE,
                                                   reverse=TRUE)) +
      ggplot2::scale_fill_manual(labels=labs$label, values=pal, drop=FALSE)

    if(.meta_added(eem)){plot_name <- eem$meta_name}else{plot_name <- eem$sample}

    if(title){
      plot <- plot + ggplot2::labs(subtitle=plot_name) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(size=9))
    }

  return(plot)
}
