#' Validate Tea Absorbance
#'
#' Validates the absorbance of sample tea standards compared to a mean of good
#' tea standards run in the past on the instrument. Returns an error if the sample
#' absorbance curve is outside the fail threshold, and a warning if inside the fail
#' threshold but not fully within historical error bars.
#'
#' @section Default tea absorbance model:
#'
#' If model = "default", the model included in the package is used to validate
#' absorbance. The included model is the repeated absorbance of a 1% Unsweetened
#' Pure Leaf Black Tea solution created following USGS SRMtea method. See
#' https://pubs.usgs.gov/of/2018/1096/ofr2018.1096.pdffor further explanation of
#' SRMtea standard.
#'
#' ...
#'
#' @param abslist abslist of sample absorbances
#' @param model the comparison model of tea absorbance
#' @param fail_threshold user adjustable threshold to determine failure of the
#' tea absorbance validation procedure. Must be a decimal between 0 and 1. The value
#' is the minimum percent of the absorbance curve that is outside the error bar
#' for validation failure. For example if the value is 0.15, then the check fails
#' if > 15% of the sample absorbance curve is outside the modeled error bars.
#' Defaults to 0.10.
#'
#' @return returns a data.frame with pass/fail results of tea standard checks
#' @export


# TODO write tests for this function
validate_tea_absorbance <- function(abslist,
                                    pattern = "tea",
                                    model = "default",
                                    fail_threshold = 0.10,
                                    ...) {

  stopifnot(.is_abslist(abslist),
            is.character(pattern))

  # Pull tea standard samples - TODO should we make this more flexible?
  tea_abs <- abs_get_tea(abslist,
                         pattern = pattern,
                         info = "sample")

  # Check there is a Tea standard somewhere in the absorbance data.frame
  if (length(tea_abs) == 0) stop("No tea standards found. - Please ensure 'pattern' argument is correct")

  # Load in tea absorbance model data.frame
  # TODO figure out a better way for user to make their own tea absorbance model
  # CSV probably isn't the best option.
  if (model == "default") {
    model_abs <- eemanalyzeR::tea_absorbance_model
  } else if(file_ext(model) == "csv") {
    model_abs <- read.csv(model)
  } else {
    stop("No valid tea absorbance model found")
  }

  # Join them together just to make sure the wavelengths are right (probably an unnecessary step)
  test_dataset <- merge(model_abs,
                        tea_abs_df,
                        by = "wavelength")
  # Define the test lower and upper bounds
  lowbound <- test_dataset$sdmin_3x
  hibound <- test_dataset$sdmax_3x

  # Create data structure to store the pct_outside_interval
  test_results <- data.frame("sample" = character(),
                             "validation_result" = logical(),
                             "pct_outside_bounds" = numeric())

  # TODO - clean this up by making sub functions and lapplying
  # Loop through
  for (sample in tea_abs) {

    sample_id <- sample$sample
    cat("Validating absorbance of tea standard ", sample_id)
    # Turn the absorbance into a data.frame
    tea_abs_df <- data.frame(sample[["data"]])
    names(tea_abs_df) <- c("wavelength",
                           "sample_absorbance")

    # Join them together just to make sure the wavelengths are right (probably an unnecessary step)
    test_dataset <- merge(model_abs,
                          tea_abs_df,
                          by = "wavelength")
    # Define the test lower and upper bounds
    lowbound <- test_dataset$sdmin_3x
    hibound <- test_dataset$sdmax_3x

    # Create data structure to store the pct_outside_interval
    test_results <- data.frame("sample" = character(),
                               "validation_result" = logical(),
                               "pct_outside_bounds" = numeric())
    # run the tests
    sample_abs <- test_dataset$sample_absorbance
    test_low <- (sample_abs - lowbound) > 0 # FALSE if lower than low boundary
    test_hi <- (sample_abs - hibound) < 0   # FALSE if higher than high boundary
    test_pass <- test_low & test_hi
    pct_outside_interval <-  1 - (sum(test_pass) / length(test_pass))

    # Throw a warning for any if the validation check has pct_inside_interval
    # >= the fail threshold - 1
    if (pct_outside_interval >= fail_threshold) {

      passfail <- "FAIL"

      # Throw a warning about the failure and print to console
      warning("Warning: ",
              sample_id,
              " was outside the absorbance testing threshold.")
      cat("Warning: ",
          sample_id,
          " was outside the absorbance testing threshold.")

      # Plot the Tea absorbance vs the validated model
      plot_absorbance_error(model_abs,
                            sample_abs,
                            sample_id,
                            condition = "WARNING")

      # User response using .yesorno
      response <- .yesorno("Do you wish to accept the warning and continue?",
                           paste0("User warned sample",
                                  sample_id,
                                  " FAILED absorbance validation checks, but ok'd to continue"),
                           "Processing Aborted by user. Tea standard failed validation.")
      # Writing the response to processing_tracking.txt
      if(response) {
        .write_processing_tracking(
          paste0("User warned sample",
                 sample_id,
                 " FAILED absorbance validation checks, but ok'd to continue"))
      } else {
        .write_processing_tracking(
          "Processing Aborted by user. Tea standard failed validation.")

        stop("Processing Aborted by user. Tea standard failed validation.")
      }
    } else {
      passfail <- "PASS"
    }


    # Save the results of the test
    new_result <- data.frame("sample" = sample_id,
                             "validation_result" = passfail,
                             "pct_failed" = pct_outside_interval * 100)
    test_results <- rbind(test_results, new_result)

  }
  return(test_results)
}

#' Plot the absorbance of a failed tea absorbance validation check.
#'
#' `validate_tea_absorbance()` uses this function to plot the tea absorbance
#' validation model vs. the sample tea absorbance that is outside the
#'
#' @param model_data tea absorbance model data
#' @param sample_abs_vector tea sample absorbance vector
#' @param sample_name name of the tea sample to use as title of plot
#' @param condition was the failure a warning or an error?
#'
#' @return plot of tea sample absorbance and model of "good" absorbance
#' @export
#'
plot_absorbance_error <- function(model_data,
                                  sample_abs_vector,
                                  sample_id,
                                  condition) {
  # Putting the data together
  all_data <- cbind(model_data, sample_abs_vector)

  # Plotting Code
  abs_plot <- ggplot2::ggplot(all_data) +
    ggplot2::geom_line(aes(x = .data$wavelength,
                           y = .data$sample_abs_vector),
                       color = "red") +
    ggplot2::geom_line(aes(x = .data$wavelength,
                           y = .data$mean_abs_by_wavelength),
                       color = "black") +
    ggplot2::geom_ribbon(aes(x = .data$wavelength,
                             ymin = .data$sdmin_3x,
                             ymax = .data$sdmax_3x),
                         alpha = 0.15) +
    ggplot2::labs(y = "Absorbance",
                  x = "Wavelength (nm)",
                  title = paste0(sample_id, " - ", condition)) +
    ggplot2::theme_bw()

  plot(abs_plot)
}
