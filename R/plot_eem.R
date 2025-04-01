#TODO: remove lower area?, clip in tighter? make as an option?


#' Plot excitation emission matrices with ggplot2
#'
#' Used to make a nice plot of one or multiple excitation emission matrices (EEMs) using ggplot2
#'
#' @param eem an \code{eem} or \code{eemlist} object containing EEM's data
#' @param nbin the number of bins used in the contour plot
#' @param equal_scale logical, should the scale be the same for all the plots in the eemlist?
#' @param pal the colors used for the fill scale, if not specified it will use the \link[pals]{parula} palette.
#' If less colors are provided than required, it will use \link[grDevices]{colorRampPalette} to fill in colors.
#'
#' @return if eem is an \code{eem} it will return a single ggplot2 object. If eem is an \code{eemlist}, it will return a list of ggplot2 objects.
#'
#' @importFrom ggplot2 labs ggplot aes geom_contour_filled coord_cartesian geom_contour guides element_text scale_fill_manual theme
#' @importFrom ggpubr ggarrange
#' @importFrom pals parula
#' @export
#' @seealso \href{https://ggplot2.tidyverse.org}{ggplot2}
#' @details
#' If you're plotting a single EEM, the object returned is a \code{ggplot2} object, so it is compatible with other ggplot2 functions to
#' further modify a plot as desired. If you're plotting multiple EEMs, the object returned is a list of \code{ggplot2} objects, which can also be
#' modified by specifying which plot you want to modify. See examples for more details.
#'
#' @examples
#'
#' eems <- add_metadata(metadata, example_eems)
#' eems <- add_blanks(eems)
#' eems <- subtract_blank(eems)
#' eems <- remove_scattering(eems)
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

plot_eem <- function(eem, nbin=8, equal_scale=FALSE, pal=NULL){
  stopifnot(.is_eem(eem) | .is_eemlist(eem))
  #if eemlist, apply plotting function to list
  if(.is_eemlist(eem)){
    #if equal_scale == TRUE, get limits
    if(equal_scale){
      z_max <- max(sapply(eem, function(x){max(x$x, na.rm=T)}))
      z_min <- min(sapply(eem, function(x){min(x$x, na.rm=T)}))
    }else{
      z_max <- NULL
      z_min <- NULL
    }


    plot <- lapply(eem, .plot_eem, nbin, z_min, z_max, pal)
    print(ggpubr::ggarrange(plotlist = plot))
    return(invisible(plot))
  }

  plot <- .plot_eem(eem, nbin=nbin, z_min=NULL, z_max=NULL, pal=pal)
  return(plot)

}

#' Plot an EEMs nicely with ggplot2
#'
#' @param eem an \code{eem} object containing EEM's data.
#' @param nbin the number of bins used in the contour plot
#' @param z_min the minimum intensity value to plot, if NULL uses the maximum value from the EEM
#' @param z_max the maximum intensity value to plot, if NULL uses the minimum value from the EEM
#' @param pal the colors used for the fill scale, if not specified it will use the \link[pals]{parula} palette
#'
#' @return a ggplot2 object
#' @noRd
#' @seealso \link[ggplot2]
#'
.plot_eem <- function(eem, nbin, z_min, z_max, pal){

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
      fill_lab <- expression(Intensity~(R.U.~L~mgC^{-1}))
    } else if(attr(eem, "is_doc_normalized")){
      fill_lab <- expression(Intensity~(L~mgC^{-1}))
    } else if(attr(eem, "is_raman_normalized")){
      fill_lab <- expression(Intensity~(R.U.))
    } else{
      fill_lab <- "Raw Intensity"
    }

    #turn into a dataframe with x,y,z
    df_plot <- data.frame(x=rep(eem$ex, each=length(eem$em)),
                          y=rep(eem$em, times=length(eem$ex)), z=as.vector(eem$x)) %>% na.omit()

    #create plot
    plot <- ggplot2::ggplot(df_plot, ggplot2::aes(x=.data$x,y=.data$y,z=.data$z)) +
      ggplot2::geom_contour_filled(breaks=breaks) +
      ggplot2::coord_cartesian(expand = FALSE) + ggplot2::geom_contour(color="black", breaks=breaks) +
      ggplot2::labs(x="Excitation (nm)", y="Emission (nm)", fill=fill_lab)  +
      ggplot2::guides(fill = ggplot2::guide_legend(title.position = "right",direction = "vertical",
                                                   title.theme = ggplot2::element_text(angle = 90, size = 12, colour = "black"),
                                                   barheight = .95, barwidth = .95,
                                                   title.hjust = 0.5, raster = FALSE,
                                                   reverse=TRUE)) +
      ggplot2::scale_fill_manual(labels=labs$label, values=pal) +
      ggplot2::theme(axis.text = ggplot2::element_text(colour = 1, size = 10),
                     axis.title = ggplot2::element_text(colour = 1, size = 12))

  return(plot)
}
