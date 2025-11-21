#' Validate the blanks
#'
#' Will run automated validation checks and plot blanks for manual validation.
#'
#' @details
#' If validate_blanks run in interactive console (most RStudio sessions), this will plot the blanks
#' using ggeem. If run non-interactively (such as in a batch processing or in package tests),
#' nothing will be plotted, but automatic checks will take place.
#'
#' @param blanklist an \code{eemlist} containing blank samples
#'
#' @return TRUE if blanks meet validation standards, FALSE if not
#' @export
#' @importFrom rlang is_interactive
#' @importFrom ggplot2 labs theme
#'
#' @examples
#' eems <- subset_qaqc(example_eems)
#' continue <- validate_blanks(eems)
validate_blanks <- function(
    blanklist) {

  #cat("Plotting blanks for user validation \n")

  # Plot the instrument blank
  if (rlang::is_interactive()) {
  blank_plot1 <- ggpubr::ggarrange(plotlist=plot(unique(blanklist)), common.legend = T, legend="right")
  blank_plot2 <- ggpubr::ggarrange(plotlist=plot(remove_scattering(unique(blanklist), type=c(T,T,T,T), interpolate=c(F,F,F,F))),
                                   common.legend = T, legend="right")

  blank_plot <- ggpubr::ggarrange(blank_plot1, blank_plot2, ncol = 1, align="h")
  print(blank_plot)
  #print("is_interactive didn't work")
  }

  # TODO - write validation data to tracking file

  # Prompt user for input to accept or decline the warning
  continue <- .yesorno("After reviewing blank(s), do you want to continue processing samples",
                       "Instrument blank accepted and added to samples",
                       "Instrument blank not accepted - Exiting")

  return(continue)

}
