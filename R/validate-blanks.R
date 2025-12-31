#' Validate the instrument blank(s)
#'
#' Plots validation instrument blank samples for manual inspection.
#'
#' @param blanklist An `eemlist` containing containing the blank EEMs.
#'
#' @return A blanklist with validated blanks, or an error if no valid blanks are selected
#'
#' @export
#' @md
#'
#' @examples
#' eems <- add_metadata(metadata, example_eems)
#' eems <- subset_type(eems, type = "iblank")
#' valid_blanklist <- validate_blanks(eems)
validate_blanks <- function(blanklist) {

  # Split the blanklist into iblanks and sblank
  iblank <- unique(subset_type(blanklist, "iblank"))
  sblank <- unique(subset_type(blanklist, "sblank"))

  # Instrument blank - First check it is valid by plotting
  iblank_valid <- plot_blank_and_ask(iblank)

  # if iblank is NOT valid try the sblanks (in sequence)
  if(!iblank_valid) {
    sblank_valid <- FALSE
    #try to validate using sblanks
    while(length(sblank) > 0 & !sblank_valid){
      try_sblank <- sblank[1]
      sblank_valid <- plot_blank_and_ask(try_sblank)

      if(sblank_valid) {
        valid_blanklist <- try_sblank

        #write to readme
        .write_readme_line(paste0("Instrument blank was replaced with analytical blank: ", try_sblank$meta_name), "eem_add_blank", NULL)

      }else {
        #remove non accepted blank and try again
        sblank[[1]] <- NULL
      }
    }
  } else {
    valid_blanklist <- iblank
    .write_readme_line("Instrument blank was visually validated and accepted using the 'validate_blanks' function", "eem_add_blank", NULL)

  }

  # What to do if no valid blanks?
  stopifnot("No valid instrument or sample blanks found. Aborting processing" = exists("valid_blanklist"))

  return(valid_blanklist)
}


#' Answer validation questions yes or no
#'
#' @importFrom rlang is_interactive
#' @noRd
.yesorno <- function(question, y_response, n_response) {

    if (!rlang::is_interactive()) {
      return(TRUE)
    }

    stopifnot(
      is.character(question),
      is.character(y_response),
      is.character(n_response)
    )

    repeat {
      cont <- readline(paste0(question, " [y/n]: "))

      if (grepl("^y$", cont, ignore.case = TRUE)) {
        message(y_response)
        return(TRUE)
      }

      if (grepl("^n$", cont, ignore.case = TRUE)) {
        message(n_response)
        return(FALSE)
      }

      message("Improper response; please respond with 'y' or 'n'.")
    }
}

#' Helper function to plot blanks for validation
#'
#' @param blanklist An `eemlist` containing containing the blank EEMs.
#'
#' @return TRUE if blank is valid, FALSE if not. Based on user input. If not run in interactive session returns TRUE
#' @noRd
plot_blank_and_ask <- function(blanklist) {
  blank_plot1 <- ggpubr::ggarrange(plotlist = plot(unique(blanklist), title="sample"), common.legend = T, legend = "right")

  blank_plot2 <- ggpubr::ggarrange(
      plotlist = plot(remove_scattering(unique(blanklist, , title="sample"), type = c(T, T, T, T), interpolate = c(F, F, F, F))),
      common.legend = T, legend = "right"
    )

  blank_plot <- ggpubr::ggarrange(blank_plot1, blank_plot2, ncol = 1, align = "h")

  # TODO - why does plotting sometimes not happen until after accepting?
    #only print if interactive, otherwise it will save a pdf we don't need
    if(rlang::is_interactive()){print(blank_plot)}

   #Prompt user for input to accept or decline the warning
   continue <- .yesorno(
     "After reviewing blank(s), do you want to continue processing samples",
     "Blank accepted and added to samples.",
     "Blank not accepted."
   )
  return(continue)
}
