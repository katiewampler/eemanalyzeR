
# TODO Function to calculate generic absorbance curve from a bunch of given "good" data
# Calculate the mean and standard deviation for each wavelength

#' Create a long term absorbance or EEM model (for tea stds and blanks)
#'
#' @param abs_dir location of absorbance data. This should be a directory with multiple
#' absorbance data files (something like )
#' @param save_path location to save absorbance model as a CSV
#' @param sd_multiplier acceptable standard deviation in data
#' @param overwrite overwrite existing model?
#'
#' @returns a absorbance model of averaged absorbance
#' @export
#' @importFrom tidyr pivot_longer



create_absorbance_model <- function(abs_dir,
                                    save_path = "data",
                                    sd_multiplier = 3, # TODO better name for this arg
                                    overwrite = FALSE) {

  stopifnot(dir.exists(abs_dir),
            dir.exists(save_path))

  # Load the "good" absorbance data. These will have to be selected by hand prior
  # to creating the model
  good_abs <- abs_dir_read(abs_dir)

  # Merge all the good abs into one data.frame
  good_abs_df <- get_sample_info(good_abs, "data")

  # Convert data to long for easy summarizing (ONLY WORKS FOR DATA.FRAME)
  good_data_long <- tidyr::pivot_longer(good_abs_df,
                                        cols = !matches("wavelength"),
                                        names_to = "sample",
                                        values_to = "absorbance") %>%
    dplyr::filter(wavelength <= 791) # removes data since there is a very high absorbance artifact at 794nm

  good_abs_model <- dplyr::group_by(good_data_long,
                                          wavelength) %>%
    dplyr::summarize(mean_abs_by_wavelength = mean(absorbance),
                     sd_abs_by_wavelength = sd(absorbance)) %>%
    dplyr::mutate(sdmin_mult = mean_abs_by_wavelength - sd_multiplier * sd_abs_by_wavelength,
                  sdmax_mult = mean_abs_by_wavelength + sd_multiplier * sd_abs_by_wavelength)
#
#   # TODO Make this part interactive?
#   # Plot good absorbance data with mean and SD ribbon
#   good_abs_plot <- ggplot2::ggplot(data = good_data_long) +
#     ggplot2::geom_line(ggplot2::aes(x = wavelength,
#                            y = absorbance,
#                            color = sample),
#                        alpha = 0.1) +
#     ggplot2::geom_line(data = good_abs_model,
#                        ggplot2::aes(x = wavelength,
#                            y = mean_abs_by_wavelength),
#                        linewidth = 1,
#                        linetype = 2,
#                        color = "black") +
#     ggplot2::geom_ribbon(data = good_abs_model,
#                          ggplot2::aes(x = wavelength,
#                              ymin = sdmin_mult,
#                              ymax = sdmax_mult),
#                          alpha = 0.2) +
#     ggplot2::theme(legend.position = "none")
#
#   plot(good_abs_plot)

  # Save the good model of tea absorbance as an rda file in data folder or a csv
  # in some generic folder
  return(good_abs_model)
}


