#' Default plot methods for optical data with ggplot2
#'
#' Used to make a nice plot of one or multiple excitation emission matrices
#' (EEMs) or absorbance spectra using ggplot2.
#'
#' @param x An `eemlist`, `eem`, `abslist`, or `abs` object.
#' @param nbin Number of bins used in the contour plot.
#' @param equal_scale Logical. If `TRUE`, sets the scale the same for all
#'   plots in an `eemlist`.
#' @param pal Color palette for the fill scale. Defaults to [pals::parula()].
#'   If fewer colors are provided than required, [grDevices::colorRampPalette()]
#'   is used to fill in colors.
#' @param title Either "none", "meta_name", or "sample" which indicates what to use for the plot title.
#' @param remove_lower Logical. If `TRUE`, sets values below the first-order
#'   Rayleigh line to `NA`, which can reduce artifacts affecting the color scale.
#' @param annotate Logical. If `TRUE`, displays index regions on EEM plots.
#' @param index_method Either "eemanalyzeR", "eemR", "usgs".
#' @param ... Additional arguments passed to `plot`.
#'
#' @md
#'
#' @return
#' - If `x` is an `eem`, `abs`, or `abslist`: a single `ggplot2` object.
#' - If `x` is an `eemlist`: a list of `ggplot2` objects.
#'
#' @seealso [ggplot2::ggplot()]
#'
#' @details
#' Single EEM plots return a `ggplot2` object, compatible with other `ggplot2`
#' modifications. Multiple EEMs return a list of `ggplot2` objects, which can
#' be individually modified. See examples for usage.
#'
#' @note
#' Slow plotting may be due to your default graphics device. See
#' <https://forum.posit.co/t/graphics-not-working-in-rstudio/149111> for guidance.
#'
#' @examples
#' eems <- example_processed_eems
#' abs <- example_processed_abs
#'
#' # plot just one eem/abs
#' plot(eems[[3]])
#' plot(abs[[3]])
#'
#' # plot all in an eemlist or abslist
#' plots <- plot(eems)
#' plots <- plot(abs)
#'
#' # change color scale
#' plot(eems, pal = c("darkblue", "lightblue"))
#'
#' # make color bar consistent across all plots
#' plot(eems[2:4], equal_scale = TRUE)
#'
#' # customize using ggplot2 commands
#' plot(eems[[2]]) + ggplot2::labs(title = "Test EEM")
#' plot(eems)[[3]] + ggplot2::labs(title = "Test EEM")
#'
#' # modify then arrange together
#' plots <- plot(eems)
#' plots[[3]] <- plots[[3]] + ggplot2::labs(title = "Test EEM")
#' print(ggpubr::ggarrange(plotlist = plots))
#'
#' # remove lower area below rayleigh line
#' plots <- plot(eems, remove_lower = TRUE)
#'
#' # annotate the plot with the peaks
#' plot(eems[[3]], annotate = TRUE)
#'
#' @export
#' @method plot eem
#' @name plot

plot.eem <- function(x, nbin = 8, equal_scale = FALSE, pal = NULL,
                     title = "none", remove_lower = FALSE,
                     annotate = FALSE, index_method = "eemanalyzeR", ...) {
  stopifnot(.is_eem(x))

  plot <- .plot_eem(x, nbin = nbin, z_min = NULL, z_max = NULL, pal = pal,
                    title=title,
                    lower = remove_lower, annotate = annotate,
                    index_method = index_method)
  return(plot)
}

#' @method plot eemlist
#' @export
#' @name plot
plot.eemlist <- function(x, nbin = 8, equal_scale = FALSE, pal = NULL, remove_lower = FALSE,
                         title = "none", annotate = FALSE, index_method = "eemanalyzeR", ...) {
  stopifnot(.is_eemlist(x))

  # if equal_scale == TRUE, get limits
  if (equal_scale) {
    z_max <- max(sapply(x, function(x) {
      max(x$x, na.rm = T)
    }))
    z_min <- min(sapply(x, function(x) {
      min(x$x, na.rm = T)
    }))
    scale <- TRUE
  } else {
    z_max <- NULL
    z_min <- NULL
    scale <- FALSE
  }
  if (annotate) {
    plot <- lapply(x, .plot_eem, nbin, z_min, z_max, pal, remove_lower, title = title, annotate = TRUE, index_method = index_method)
  } else {
    plot <- lapply(x, .plot_eem, nbin, z_min, z_max, pal, remove_lower, title = title)
  }
  names(plot) <- get_sample_info(x, "sample")

  # only show if interactive to prevent a pdf from being written
  if (is_interactive()) {
    print(ggpubr::ggarrange(plotlist = plot, common.legend = scale, legend = "right"))
  } else {
    ggpubr::ggarrange(plotlist = plot, common.legend = scale, legend = "right")
  }

  return(invisible(plot))
}

#' @method plot abs
#' @export
#' @name plot
plot.abs <- function(x, pal = NULL, ...) {
  stopifnot(.is_abs(x))

  # format for plotting
  abs <- as.data.frame(x$data)
  colnames(abs) <- c("wavelength", "abs")
  abs$sample <- get_sample_info(x, "sample")

  plot <- ggplot2::ggplot(data = abs, aes(x = .data$wavelength, y = .data$abs, color = .data$sample)) +
    ggplot2::geom_line(linewidth = 1) +
    labs(x = "Wavelength (nm)", y = "Absorbance (AU)", color = "Sample")

  # add custom colors if specified
  if (!is.null(pal)) {
    if (length(pal) < length(x)) {
      pal <- grDevices::colorRampPalette(pal)(length(x))
    }

    plot <- plot + ggplot2::scale_color_manual(values = pal, drop = FALSE)
  }


  return(plot)
}

#' @method plot abslist
#' @export
#' @name plot
plot.abslist <- function(x, pal = NULL, ...) {
  stopifnot(.is_abslist(x))

  abs <- get_sample_info(x, "data")
  abs <- as.data.frame(abs)
  abs <- abs %>% pivot_longer(-"wavelength", names_to = "sample", values_to = "abs")

  plot <- ggplot2::ggplot(data = abs, aes(x = .data$wavelength, y = .data$abs)) +
    ggplot2::geom_line(linewidth = 1) +
    labs(x = "Wavelength (nm)", y = "Absorbance (AU)") + facet_wrap(~sample, scales="free")

  # add custom colors if specified
  if (!is.null(pal)) {
    if (length(pal) < length(x)) {
      pal <- grDevices::colorRampPalette(pal)(length(x))
    }

    plot <- plot + ggplot2::scale_color_manual(values = pal, drop = FALSE)
  }

  return(plot)
}
