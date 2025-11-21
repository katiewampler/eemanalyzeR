#' Export Processed Optics Data
#'
#' @param eemlist an \code{eemlist} object containing EEM's data.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param filename a character, creates a folder with this name within the \code{output_dir} and uses this name for saving file names
#' @param output_dir the location to save the data to. if no location is specified it will save to the temporary directory
#' @param meta optional, a \code{data.frame} of metadata
#' @param plot optional, if \code{NULL} plots will not be exported. If a list of plots are included
#' these will be exported as .png files
#' @param indices optional, if \code{NULL} indices will not be exported. If a list of indices are included
#' these will be exported
#' @param csv logical, if TRUE processed EEMs and absorbance data will be written to the \code{output_dir} as .csv files
#'
#' @importFrom ggplot2 ggsave
#' @importFrom utils write.table
#' @export
#'
#' @return A list of the processed data which is also saved to the output directory specified.
#' The list contains the following items:
#' \itemize{
#' \item{eemlist: the \code{eemlist}}
#' \item{abslist: the \code{abslist}}
#' \item{readme: a character vector containing information about the data processing steps}
#' \item{meta: the metadata associated with the samples, may be \code{NULL} if not provided}
#' \item{indices: a list of EEMs and absorbance indices, may be \code{NULL} if not provided}
#' \item{plots: a list of EEMs plots, may be \code{NULL} if not provided}
#' }
#'
#' @examples
#' plots <- plot(example_processed_eems)
#' indices <- get_indices(example_processed_eems, example_processed_abs)
#' data <- export_data(example_processed_eems, example_processed_abs,
#'     filename="eemanalyzeR_example",
#'     indices = indices, plot=plots, meta=metadata)
export_data <- function(eemlist, abslist, filename, output_dir=NULL,
                        meta=NULL, indices=NULL,
                        plot=NULL, csv=FALSE){
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist), is.data.frame(meta) | is.null(meta),
            is.list(indices) | is.null(indices), is.list(plot) | is.null(plot),
            is.logical(csv), is.character(filename))

  #check if processing has been done, not warn that indices may be unreliable
    steps <- check_processing(eemlist)
    steps <- steps[-nrow(steps),] #remove check for DOC

    if(all(!steps$done)){
      warning("Data has not been processed. \nIt's recomended to use eemanalyzeR::process_eem to process EEMs before saving.")
    }else if(any(!steps$done)){
      missing <- steps[steps$done == FALSE,]
      warning("Data has not been fully processed. The following processing steps are missing:\n",
              paste(missing$warning, collapse="\n"), "\n\nIt's recomended to use eemanalyzeR::process_eem to process EEMs before saving.")}


  #if no output_dir is specified get the temp directory
    if(is.null(output_dir)){output_dir <- tempdir()}
    if(!dir.exists(file.path(output_dir, filename))){
      dir.create(file.path(output_dir, filename))
    }

  #TODO: replace with package envir
    readme <- get("readme", envir = .GlobalEnv)

  #convert everything to list and save
    output <- list(eemlist=eemlist, abslist=abslist,
                   readme=readme, metadata=meta,
                   indices=indices, plots=plot)

    saveRDS(output, file.path(output_dir, filename, paste0("processed_data_", filename,".RDS")))

  #save readme as text file (add initial information about package to it)
    write.table(capture.output(.print_readme()), file.path(output_dir, filename, paste0("readme_", filename,".txt")),
                quote=FALSE, col.names = FALSE, row.names = FALSE)

  #save plots as png (default is yes)
    if(!is.null(plot)){
      #figure out dim for summary plot
        height <- ceiling(length(plot)/4)
        width <- ceiling(length(plot)/height)

      summary <- ggpubr::ggarrange(plotlist = plot, nrow=height, ncol=width)
      ggplot2::ggsave(filename = paste0("summary_plots_", filename, ".png"),
                      path = file.path(output_dir, filename),
                      plot = summary,
                      units = "cm",
                      height = height*13,
                      width=17*width,
                      limitsize = FALSE)

     lapply(names(plot), function(name) {
        file <- paste0(name, ".png")
        ggplot2::ggsave(filename = file,
                        path = file.path(output_dir, filename),
                        plot = plot[[name]],
                        units = "cm",
                        height = 13,
                        width=17)})

    }

  #save indices as .csv
    if(!is.null(indices)){
      write.csv(indices$abs_index, file.path(output_dir,filename, paste0("absindices_", filename, ".csv")),
                row.names=FALSE, quote=FALSE)
      write.csv(indices$eem_index, file.path(output_dir,filename, paste0("fluorindices_", filename, ".csv")),
                row.names=FALSE, quote=FALSE)
    }

  #save eemlist and abslist as csv (default is no)
    if(csv == TRUE){
      lapply(eemlist, function(x){
        name <- paste0(x$sample, "_processed.csv")
        data <- get_sample_info(x, "x")
        write.csv(data, file.path(output_dir, filename, name), row.names = TRUE)
      })

      name <- paste0("absorbance_processed_", filename, ".csv")
      data <- get_sample_info(abslist, "data")
      write.csv(data, file.path(output_dir, filename, name), row.names = FALSE)

    }

}
