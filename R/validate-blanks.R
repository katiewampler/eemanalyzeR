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
#' @param plotting_info TODO - plotting arguments for ggeem (not implemented yet)
#' @param blank_model TODO - model of blank noise calculated from multiple runs averaged
#'                    together for automated validation.
#'
#' @return TRUE if blanks meet validation standards, FALSE if not
#' @export
#' @importFrom staRdom ggeem
#' @importFrom rlang is_interactive
#'
#' @examples
#' continue <- validate_blanks(eem_get_blank(example_eems))
validate_blanks <- function(
    blanklist,
    plotting_info = NULL, # Placeholder for arguments to ggeem
    blank_model = longterm_blank) {

  #cat("Plotting blanks for user validation \n")

  # Plot the instrument blank
  if (rlang::is_interactive()) {
  plot_data <- lapply(eem_normalize(blanklist), eem_subtract, eem_normalize(blank_model))
  class(plot_data) <- "eemlist"

  blank_plot <- staRdom::ggeem(plot_data) + ggplot2::labs(title="Deviation of Blank Sample(s) from Long-Term Average")
  print(blank_plot)
  print("is_interactive didn't work")
  }

  # TODO Compare to Blank model
  if (!is.null(blank_model)) {
    # do some kind of blank signal comparison? dunno what kind yet
  }

  # TODO - write validation data to tracking file

  # Prompt user for input to accept or decline the warning
  continue <- .yesorno("After reviewing blank(s), do you want to continue processing samples",
                       "Instrument blank accepted and added to samples",
                       "Instrument blank not accepted - Exiting")

  return(continue)

}
