#' Validate the instrument blank(s)
#'
#' Plots validation instrument blank samples for manual inspection.
#'
#' @param blanklist An `eemlist` containing containing the blank EEMs.
#'
#' @return TRUE if blanks meet validation standards, FALSE if they do not.
#'
#' @export
#' @md
#'
#' @examples
#' eems <- add_metadata(metadata, example_eems)
#' eems <- subset_type(eems, type = "iblank")
#' continue <- validate_blanks(eems)
validate_blanks <- function(blanklist) {

  # Plot the instrument blank
  if (rlang::is_interactive()) {
    blank_plot1 <- ggpubr::ggarrange(plotlist = plot(unique(blanklist)), common.legend = T, legend = "right")
    blank_plot2 <- ggpubr::ggarrange(
      plotlist = plot(remove_scattering(unique(blanklist), type = c(T, T, T, T), interpolate = c(F, F, F, F))),
      common.legend = T, legend = "right"
    )

    blank_plot <- ggpubr::ggarrange(blank_plot1, blank_plot2, ncol = 1, align = "h")
    print(blank_plot)
    # print("is_interactive didn't work")
  }

  # TODO - write validation data to tracking file

  # Prompt user for input to accept or decline the warning
  continue <- .yesorno(
    "After reviewing blank(s), do you want to continue processing samples",
    "Instrument blank accepted and added to samples",
    "Instrument blank not accepted - Exiting"
  )

  return(continue)
}
