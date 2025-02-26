
# TODO Function to calculate generic absorbance curve from a bunch of given "good" data
# Max good absorbance at 239 nm is 0.2166726 (USGS TEA STANDARD)
# Min good absorbance at 239 nm is 0.1430418 (USGS TEA STANDARD)
# Calculate the mean and standard deviation for each wavelength

#' Create a long term absorbance or EEM model (for tea stds and blanks)
#'
#' @param path_to_abs location of absorbance data
#' @param save_path location to save absorbance model
#' @param sd_multiplier accetable standard deviation in data
#' @param overwrite overwrite exisiting model?
#'
#' @returns a absorbance model of averaged absorbance
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom usethis use_data
#' @noRd
save_absorbance_model <- function(path_to_abs,
                                  save_path = "data",
                                  sd_multiplier = 3, # TODO better name for this arg
                                  overwrite = FALSE) {

  # TODO check inputs for errors

  # Load the "good" absorbance data. These will have to be selected by hand prior
  # to creating the model
  good_abs <- abs_dir_read(path_to_abs)

  # TODO validate all the Abs data has the same wavelengths
  good_abs <- good_abs[good_abs$wavelength <= 791,]

  # Convert data to long for easy summarizing (ONLY WORKS FOR DATA.FRAME)
  good_data_long <- tidyr::pivot_longer(good_abs,
                                        cols = !matches("wavelength"),
                                        names_to = "sample",
                                        values_to = "absorbance")

  good_abs_model <- dplyr::group_by(good_data_long,
                                          wavelength) %>%
    dplyr::summarize(mean_abs_by_wavelength = mean(absorbance),
                     sd_abs_by_wavelength = sd(absorbance)) %>%
    dplyr::mutate(sdmin_mult = mean_abs_by_wavelength - sd_multiplier * sd_abs_by_wavelength,
                  sdmax_mult = mean_abs_by_wavelength + sd_multiplier * sd_abs_by_wavelength)

  # TODO Make this part interactive?
  # Plot good absorbance data with mean and SD ribbon
  good_abs_plot <- ggplot2::ggplot(data = good_data_long) +
    ggplot2::geom_line(aes(x = wavelength,
                           y = absorbance,
                           color = sample),
                       alpha = 0.1) +
    ggplot2::geom_line(data = good_abs_model,
                       aes(x = wavelength,
                           y = mean_abs_by_wavelength),
                       linewidth = 1,
                       linetype = 2,
                       color = "black") +
    ggplot2::geom_ribbon(data = good_abs_model,
                         aes(x = wavelength,
                             ymin = sdmin_mult,
                             ymax = sdmax_mult),
                         alpha = 0.2) +
    ggplot2::theme(legend.position = "none")

  plot(good_abs_plot)

  # Save the good model of tea absorbance as an rda file in data folder or a csv
  # in some generic folder
  if (save_path == "data") { usethis::use_data(good_abs_model, overwrite = overwrite) }
  else { write.csv(good_abs_model, file = save_path, append = !overwrite) }

}
