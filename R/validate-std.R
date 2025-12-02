#' Visually validate the check standard
#'
#' Visually inspect plots of the tea check standard absorbance compared to the long-term standard.
#'
#' @param abslist An `abslist` object.
#' @param qaqc_dir File path to the QAQC files generated with [create_mdl()] and [create_std()].
#' Default is a user-specific data directory [rappdirs::user_data_dir()].
#' @param tolerance Maximum percent deviation that the check standard can vary
#'   from the long-term values without being flagged.
#'
#' @return A `ggplot` object showing the absorbance of the check standards,
#' the long-term standard (dashed line), and the tolerance thresholds (gray ribbon).
#'
#' @export
#' @md
#'
#' @seealso [create_std()]
#'
#' @examples
#' abslist <- add_metadata(metadata, example_abs)
#' validate_std(abslist, system.file("extdata", package = "eemanalyzeR"))
validate_std <- function(abslist, qaqc_dir=NULL, tolerance=0.2){
  stopifnot(.is_abslist(abslist))

  #specify qaqc dir if not specified
  if(is.null(qaqc_dir)){qaqc_dir = .qaqc_dir()}

  #check if sample has any check standards if not warning
    check <- subset_type(abslist, type="check")
    if(length(check)==0){
      warning("No check standard samples found")
      return()
    }

  #get long-term check standard
    if(!file.exists(file.path(qaqc_dir, "abs-check-std.rds"))){
      warning("Check standard files are missing, check standards will not be checked against the long-term standard")

      #prepare data for plotting
      check_plot <- get_sample_info(check, "data") %>% as.data.frame() %>% pivot_longer(-"wavelength", names_to = "sample", values_to="abs")

      #plot check with check standard
      plot <- ggplot() +
        geom_line(data = check_plot, aes(x = .data$wavelength, y = .data$abs, color=.data$sample), linewidth=1) +
        labs(x="Wavelength (nm)", y="Absorbance (AU)", color="Sample")

    }else{
      abs_std <- readRDS(file.path(qaqc_dir, "abs-check-std.rds"))

      #prepare data for plotting
      std_plot <- as.data.frame(abs_std$data) %>% mutate(min = .data$V2*(1-tolerance), max=.data$V2*(1+tolerance))
      check_plot <- get_sample_info(check, "data") %>% as.data.frame() %>% pivot_longer(-"wavelength", names_to = "sample", values_to="abs")

      #plot check with check standard
      plot <- ggplot() + geom_ribbon(data=std_plot, aes(x=.data$V1, ymin=.data$min, ymax=.data$max), alpha=0.4) +
        geom_line(data=std_plot, aes(x=.data$V1, y=.data$V2), linetype="dashed") +
        geom_line(data = check_plot, aes(x = .data$wavelength, y = .data$abs, color=.data$sample), linewidth=1) +
        labs(x="Wavelength (nm)", y="Absorbance (AU)", color="Sample")
    }

    print(plot)

    return(plot)

}

