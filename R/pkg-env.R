# Functions around setting up a processing environment
# Need to make these so it cleans up the processing code and makes passing
# function arguments much more clear.

# TODO PUT EVERY TOP LEVEL ARGUMENT IN HERE
# if we get all the EEMs processing arguments in here, it leaves us able to handle
# all ggplot arguments using (...)

# -[x] input_dir
# -[x] output_dir
# -[x] metadata filename
# -[] processing tracking file - THIS MIGHT NEED CHANGING
# -[] noise value
# -[] interactive environment? Maybe rlang takes care of this already
# -[] processing information
# -[]
# -[]
# -[]
# -[]


# Create an empty environment to store EEMS processing arguments and parameters
.pkgenv <- new.env(parent = emptyenv())

# Path to Folder containing raw EEMs and absorbance data (input_dir) ----
# Initialize
# TODO Is there a saner default?
.pkgenv$input_dir <- ""
# Getter for input_dir
get_input_dir <- function() {
  .pkgenv$input_dir
}
# Setter for input_dir
# Setter invisibly returns the old value so we can assign it to a variable if we
# want to save it and reset to the old value later
set_input_dir <- function(filepath) {
  old <- .pkgenv$input_dir
  .pkgenv$input_dir <- filepath
  invisible(old)
}

# Path to Directory where processed data should be saved
# Initialize
# TODO Is there a saner default?
.pkgenv$output_dir <- ""
# Getter for input_dir
get_output_dir <- function() {
  .pkgenv$output_dir
}
# Setter for output_dir
# Setter invisibly returns the old value so we can assign it to a variable if we
# want to save it and reset to the old value later
set_output_dir <- function(filepath) {
  old <- .pkgenv$output_dir
  .pkgenv$output_dir <- filepath
  invisible(old)
}


# Metadata filename ----
# Initialize
.pkgenv$metadata_file <- ""
# Getter for metadata_file
get_metadata_file <- function() {
  .pkgenv$metadata_file
}
# Setter for metadata_file
set_metadata_file <- function(filename) {
  old <- .pkgenv$metadata_file
  .pkgenv$metadata_file <- filename
  invisible(old)
}

# Warnings list ----
# A list of all warnings that have been thrown during data loading and processing
.pkgenv$warnings_list <- list()
# Append warnings to the warnings list
append_warning <- function(condition_message) {
  old <- .pkgenv$warnings_list
  .pkgenv$warnings_list <- append(.pkgenv$warnings_list, condition_message)
  invisible(old)
}


# DOC File info
# Initialize to NULL
.pkgenv$doc_file <- NULL
# Getter for DOC file


# TODO can I make the DOC file a class with all the required info like
# filename, sheet name, doc_column, name_column, nskip, doc_delim, etc.. so I don't
# have to individually have all those parameters in the environment?






# Processing Tracking File ----
# Initialize the tracking file argument
# TODO update process file to R object (not external connection)
.pkgenv$process_file <- ""

# Getter for the tracking file info
get_process_file <- function() {
  .pkgenv$process_file
}

#Setter
set_process_file <- function(filepath) {
  old <- .pkgenv$process_file
  .pkgenv$process_file <- filepath
  invisible(old)
}


