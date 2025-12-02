#' Export processed absorbance and fluorescence data
#'
#' Exports processed EEM and absorbance data, plots, indices, and metadata to a
#' specified output directory. Creates a folder named by `filename` and saves all specified outputs there.
#'
#' @param eemlist An `eemlist` object.
#' @param abslist An `abslist` object.
#' #TODO Ryan doesn't understand the arguments filename and output_dir
#' @param filename A character string. Creates a folder with this name
#'   within `output_dir` and uses it for file names.
#' @param output_dir Path to save the data. Defaults to a temporary directory
#'   if not specified.
#' @param meta A `data.frame` containing sample metadata.
#' @param eem_plot If `NULL`, EEM plots will not be exported. If a
#'   list of plots is provided, they are saved as `.png` files. See [plot.eemlist()] for plotting.
#' @param abs_plot If `NULL`, absorbance plots will not be exported.
#'   If a plot is provided, it is saved as a `.png` file. See [plot.abslist()] for plotting.
#' @param indices If `NULL`, indices will not be exported. If a list of
#'   indices is provided, it is saved.
#' @param csv Logical. If `TRUE`, processed EEM and absorbance data are
#'   written to `output_dir` as `.csv` files.
#'
#' @export
#' @md
#'
#' @return A list of the processed data which is also saved to the output directory specified.
#' The list contains:
#'  - **eemlist:** the `eemlist`
#'  - **abslist:** the `abslist`
#'  - **readme:** a character vector containing information about the data processing steps
#'  - **meta:** the metadata associated with the samples, may be `NULL` if not provided
#'  - **indices:** a list of EEMs and absorbance indices, may be `NULL` if not provided
#'  - **eem_plot:** a list of EEMs plots, may be `NULL` if not provided
#'  - **abs_plot:** a ggplot2 object of the absorbance data, may be `NULL` if not provided
#'
#' @examples
#' eem_plots <- plot(example_processed_eems)
#' abs_plot <- plot(example_processed_abs)
#'
#' indices <- get_indices(example_processed_eems, example_processed_abs)
#' data <- export_data(
#'   eemlist = example_processed_eems,
#'   abslist = example_processed_abs,
#'   filename = "eemanalyzeR_example",
#'   indices = indices,
#'   eem_plot = eem_plots,
#'   abs_plot = abs_plot,
#'   meta = metadata
#' )
export_data <- function(eemlist, abslist, filename, output_dir = NULL,
                        meta = NULL, indices = NULL,
                        eem_plot = NULL, abs_plot = NULL, csv = FALSE) {
  stopifnot(
    .is_eemlist(eemlist), .is_abslist(abslist), is.data.frame(meta) | is.null(meta),
    is.list(indices) | is.null(indices), is.list(eem_plot) | is.null(eem_plot),
    is.logical(csv), is.character(filename), inherits(abs_plot, "ggplot") | is.null(abs_plot)
  )

  # check if processing has been done, not warn that indices may be unreliable
  steps <- check_processing(eemlist)
  steps <- steps[-nrow(steps), ] # remove check for DOC

  if (all(!steps$done)) {
    warning("Data has not been processed. \nIt's recomended to use eemanalyzeR::process_eem to process EEMs before saving.")
  } else if (any(!steps$done)) {
    missing <- steps[steps$done == FALSE, ]
    warning(
      "Data has not been fully processed. The following processing steps are missing:\n",
      paste(missing$warning, collapse = "\n"), "\n\nIt's recomended to use eemanalyzeR::process_eem to process EEMs before saving."
    )
  }


  # if no output_dir is specified get the temp directory
  if (is.null(output_dir)) {
    output_dir <- tempdir()
  }
  if (!dir.exists(file.path(output_dir, filename))) {
    dir.create(file.path(output_dir, filename))
  }

  # TODO: replace with package envir
  readme <- get_readme()

  # convert everything to list and save
  output <- list(
    eemlist = eemlist, abslist = abslist,
    readme = readme, metadata = meta,
    indices = indices, eem_plot = eem_plot,
    abs_plot = abs_plot
  )

  saveRDS(output, file.path(output_dir, filename, paste0("processed_data_", filename, ".rds")))

  # save readme as text file (add initial information about package to it)
  write.table(capture.output(print_readme()), file.path(output_dir, filename, paste0("readme_", filename, ".txt")),
    quote = FALSE, col.names = FALSE, row.names = FALSE
  )

  # save eem plots as png
  if (!is.null(eem_plot)) {
    # figure out dim for summary plot
    height <- ceiling(length(eem_plot) / 4)
    width <- ceiling(length(eem_plot) / height)

    summary <- ggpubr::ggarrange(plotlist = eem_plot, nrow = height, ncol = width)
    ggplot2::ggsave(
      filename = paste0("summary_plots_", filename, ".png"),
      path = file.path(output_dir, filename),
      plot = summary,
      units = "cm",
      height = height * 13,
      width = 17 * width,
      limitsize = FALSE
    )

    lapply(names(eem_plot), function(name) {
      file <- paste0(name, ".png")
      ggplot2::ggsave(
        filename = file,
        path = file.path(output_dir, filename),
        plot = eem_plot[[name]],
        units = "cm",
        height = 13,
        width = 17
      )
    })
  }

  # save absorbance plots
  if (!is.null(abs_plot)) {
    ggplot2::ggsave(
      filename = paste0("absorbance_plot_", filename, ".png"),
      path = file.path(output_dir, filename),
      plot = abs_plot,
      units = "cm",
      height = 13,
      width = 17,
      limitsize = FALSE
    )
  }
  # save indices as .csv
  if (!is.null(indices)) {
    write.csv(indices$abs_index, file.path(output_dir, filename, paste0("absindices_", filename, ".csv")),
      row.names = FALSE, quote = FALSE
    )
    write.csv(indices$eem_index, file.path(output_dir, filename, paste0("fluorindices_", filename, ".csv")),
      row.names = FALSE, quote = FALSE
    )
  }

  # save eemlist and abslist as csv (default is no)
  if (csv == TRUE) {
    lapply(eemlist, function(x) {
      name <- paste0(x$sample, "_processed.csv")
      data <- get_sample_info(x, "x")
      write.csv(data, file.path(output_dir, filename, name), row.names = TRUE)
    })

    name <- paste0("absorbance_processed_", filename, ".csv")
    data <- get_sample_info(abslist, "data")
    write.csv(data, file.path(output_dir, filename, name), row.names = FALSE)
  }
  # TODO add return exit status
}
