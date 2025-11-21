#' Default plot methods for optical data with ggplot2
#'
#' Used to make a nice plot of one or multiple excitation emission matrices (EEMs) or absorbance spectra
#' using ggplot2.
#'
#' @param x an `eem`, `eemlist`, `abs`, or `abslist` object containing EEM's or absorbance data
#' @param nbin the number of bins used in the contour plot
#' @param equal_scale logical, should the scale be the same for all the plots in the eemlist?
#' @param pal the colors used for the fill scale, if not specified it will use the \link[pals]{parula} palette.
#' If less colors are provided than required, it will use \link[grDevices]{colorRampPalette} to fill in colors.
#' @param remove_lower logical, should the area below the first order rayleigh line be set to NA values? This is helpful if there are artifacts in
#' this region that are affecting the color scale.
#' @param annotate logical, should plots show the index regions on EEM's plot?
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#' @param ... extra arguments for `plot`
#' @md
#' @return if `x` is an `eem`, `abs`, or `abslist` it will return a single ggplot2 object. If `x` is an `eemlist` it will return a list of ggplot2 objects.
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
#' abs <- example_processed_abs
#'
#' #plot just one eem/abs
#'   plot(eems[[3]])
#'   plot(abs[[3]])
#'
#' #plot all in an eemlist or abslist
#'   plots <- plot(eems)
#'   plots <- plot(abs)
#'
#' #change color scale
#'   plot(eems, pal=c("darkblue", "lightblue"))
#'   plot(abs, pal=c("darkblue", "lightblue"))
#'
#' #make color bar consistent across all plots
#'   plot(eems, equal_scale=TRUE)
#'
#' #customize using ggplot2 commands
#'   plot(eems[[2]]) + ggplot2::labs(title="Test EEM")
#'   plot(eems)[[3]] + ggplot2::labs(title="Test EEM")
#'
#' #modify then arrange together
#'   plots <- plot(eems)
#'   plots[[3]] <- plots[[3]] + ggplot2::labs(title="Test EEM")
#'   print(ggpubr::ggarrange(plotlist = plots))
#'
#'#remove lower area below rayleigh line
#'   plots <- plot(eems, remove_lower=TRUE)
#'
#' #annotate the plot with the peaks
#'   plot(eems[[3]], annotate=TRUE)
#'
#' @export
#' @method plot eem
#' @rdname plot

plot.eem <- function(x, nbin=8, equal_scale=FALSE, pal=NULL, remove_lower = FALSE,
                     annotate=FALSE, index_method="eemanalyzeR", ...){
  stopifnot(.is_eem(x))

  plot <- .plot_eem(x, nbin=nbin, z_min=NULL, z_max=NULL, pal=pal, lower=remove_lower, annotate = annotate, index_method=index_method)
  return(plot)

}

#' @method plot eemlist
#' @export
#' @rdname plot
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

#' @method plot abs
#' @export
#' @rdname plot
plot.abs <- function(x, pal=NULL, ...){
  stopifnot(.is_abs(x))

  #format for plotting
  abs <- as.data.frame(x$data)
  colnames(abs) <- c("wavelength", "abs")
  abs$sample <- get_sample_info(x, "sample")

  plot <- ggplot2::ggplot(data = abs, aes(x = .data$wavelength, y = .data$abs, color=.data$sample)) +
    ggplot2::geom_line(linewidth=1) + labs(x="Wavelength (nm)", y="Absorbance (AU)", color="Sample")

  #add custom colors if specified
  if(!is.null(pal)){
    if(length(pal) < length(x)){pal <- grDevices::colorRampPalette(pal)(length(x))}

    plot <- plot + ggplot2::scale_color_manual(values=pal, drop=FALSE)}


  return(plot)
}

#' @method plot abslist
#' @export
#' @rdname plot
plot.abslist <- function(x, pal=NULL, ...){
  stopifnot(.is_abslist(x))

  abs <- get_sample_info(x, "data")
  abs <- abs %>% pivot_longer(-"wavelength", names_to = "sample", values_to="abs")

  plot <- ggplot2::ggplot(data = abs, aes(x = .data$wavelength, y = .data$abs, color=.data$sample)) +
    ggplot2::geom_line(linewidth=1) + labs(x="Wavelength (nm)", y="Absorbance (AU)", color="Sample")

  #add custom colors if specified
    if(!is.null(pal)){
      if(length(pal) < length(x)){pal <- grDevices::colorRampPalette(pal)(length(x))}

      plot <- plot + ggplot2::scale_color_manual(values=pal, drop=FALSE)}

  return(plot)
}
