#' Plot an EEM with ggplot2
#'
#' @param eem An `eem` object or an object containing EEM data.
#' @param nbin Number of bins used in the contour plot.
#' @param z_min Minimum intensity value to plot. If `NULL`, uses the minimum
#'   value from the EEM.
#' @param z_max Maximum intensity value to plot. If `NULL`, uses the maximum
#'   value from the EEM.
#' @param pal Color palette for the fill scale. Defaults to [pals::parula()].
#' @param title Logical. If `TRUE`, includes the sample name on the plot.
#'   Attempts to use `meta_name`, falling back to `sample` if missing.
#' @param annotate Logical. If `TRUE`, shows index regions on the plot.
#' @param index_method Character. Specifies index method for annotations.
#'   Currently supports "eemanalyzeR", "eemR", and "usgs".
#'
#' @return A `ggplot2` object.
#'
#' @seealso [ggplot2]
#' @noRd
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
  browser()
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

#' Annotate EEM plot with location of indices
#'
#' Adds labels indicating the regions of an excitation–emission matrix (EEM)
#' that are used to calculate fluorescence indices. The annotation is
#' customized according to the selected index method.
#'
#' @param plot A `ggplot2` object of an `eem`.
#' @param index_method Either "eemanalyzeR", "eemR", "usgs".
#'
#' @return A `ggplot2` object.
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
