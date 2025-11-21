#' Visually Validate the Check Standard
#'
#' Visually inspect plots of the tea check standards compared to the [long-term standard][create_tea_std()].
#'
#' @param abslist an object of class `abslist`
#' @param file path to the QAQC files generated with [`create_mdl`][create_mdl()] and [`create_tea_std`][create_tea_std()],
#' default is a user-specific data directory ([`user_data_dir`][rappdirs::user_data_dir])
#' @param tolerance what is the maximum percentage the tea standard can vary from the long-term values without being flagged?
#' @md
#'
#' @returns a `ggplot` object with the absorbance of the check standards plotted with the long-term tea standard (dashed line), and the
#' tolerance thresholds (gray ribbon).
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, example_abs)
#' validate_check_std(abslist, system.file("extdata", package = "eemanalyzeR"))
validate_check_std <- function(abslist, std_dir=.qaqc_dir(), tolerance=0.2){
  stopifnot(.is_abslist(abslist))

  #check if sample has any tea samples if not warning
    check <- subset_type(abslist, type="check")
    if(length(check)==0){
      warning("No tea check standard samples found")
      return()
    }

  #get long-term tea standard
    if(!file.exists(file.path(std_dir, "abs-tea-std.rds"))){
      warning("tea check standard files are missing, check standards will not be checked against the long-term standard")
    }else{
      abs_std <- readRDS(file.path(std_dir, "abs-tea-std.rds"))
    }

  #prepare data for plotting
    std_plot <- as.data.frame(abs_std$data) %>% mutate(min = V2*(1-tolerance), max=V2*(1+tolerance))
    check_plot <- get_sample_info(check, "data") %>% pivot_longer(-"wavelength", names_to = "sample", values_to="abs")

  #plot tea with tea standard
    plot <- ggplot() + geom_ribbon(data=std_plot, aes(x=.data$V1, ymin=.data$min, ymax=.data$max), alpha=0.4) +
                       geom_line(data=std_plot, aes(x=.data$V1, y=.data$V2), linetype="dashed") +
                       geom_line(data = check_plot, aes(x = .data$wavelength, y = .data$abs, color=.data$sample), linewidth=1) +
                       labs(x="Wavelength (nm)", y="Absorbance (AU)", color="Sample")

    print(plot)

    return(plot)

}

