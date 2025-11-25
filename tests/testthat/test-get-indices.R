
  test_that("missing wavelengths don't break indices code", {
    abslist <- example_processed_abs
    eemlist <- example_processed_eems
    qaqc_dir <- system.file("extdata", package = "eemanalyzeR")
    eemlist <- eemR::eem_cut(eemlist, ex=400:900, em=400:900, exact=F) #cut down so there's missing wavelengths

    expect_no_error(indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir))

          })

  test_that("removing doc normalized data works", {
    abslist <- example_processed_abs
    eemlist <- example_processed_eems
    qaqc_dir <- system.file("extdata", package = "eemanalyzeR")
    doc <- ifelse(is.na(get_sample_info(eemlist, "doc_mgL")), 1, get_sample_info(eemlist, "doc_mgL"))
    eemlist_doc <- eem_normalize(eemlist, doc)
    eemlist_doc[3] <- lapply(eemlist_doc[3], function(x){
                               attr(x,"is_doc_normalized") <- TRUE
                               return(x)})

    expect_false(.eem_equal(eemlist[[3]]$x, eemlist_doc[[3]]$x)) #ensure normalization is done

    indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir)
    indices2 <- get_indices(eemlist_doc, abslist, qaqc_dir=qaqc_dir)

    expect_equal(indices$eem_index, indices2$eem_index)
  })

  test_that("format is correct", {
    abslist <- example_processed_abs
    eemlist <- example_processed_eems
    qaqc_dir <- system.file("extdata", package = "eemanalyzeR")

    indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir)
    expect_length(indices, 2)
    expect_s3_class(indices$abs_index, "data.frame")
    expect_s3_class(indices$eem_index, "data.frame")

    #for long
    expect_equal(ncol(indices$abs_index), 5)
    expect_equal(ncol(indices$eem_index), 5)
    expect_equal(class(indices$abs_index$value), "numeric")
    expect_equal(class(indices$eem_index$value), "numeric")
    expect_equal(class(indices$abs_index$QAQC_flag), "character")
    expect_equal(class(indices$abs_index$QAQC_flag), "character")
    expect_false(any(is.na(indices$abs_index$value)))
    expect_false(any(is.na(indices$eem_index$value)))
    expect_false(any(is.na(indices$abs_index$QAQC_flag)))
    expect_false(any(is.na(indices$eem_index$QAQC_flag)))


    #for wide
    indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir, return="wide")
    expect_equal(nrow(indices$abs_index), 4)
    expect_equal(nrow(indices$eem_index), 4)
    expect_true(all(apply(indices$abs_index, 1, class) == "character"))
    expect_true(all(apply(indices$eem_index, 1, class) == "character"))

    expect_length(unique(indices$eem_index$meta_name), 4)
    expect_length(unique(indices$abs_index$meta_name), 4)

    expect_length(unique(indices$abs_index$sample_name), 4)
    expect_length(unique(indices$eem_index$sample_name), 4)


  })

  test_that("NA index doesn't break anything",{
    return_NA <- function(eemlist, abslist, cuvle=1, qaqc_dir=NULL){
      return(list(abs_index=NA, eem_index=NA))}

    qaqc_dir <- system.file("extdata", package = "eemanalyzeR")

    expect_no_error(get_indices(example_processed_eems, example_processed_abs, index_method=return_NA, qaqc_dir = qaqc_dir))
    expect_no_error(get_indices(example_processed_eems, example_processed_abs, index_method=return_NA, qaqc_dir = qaqc_dir))

  })

  test_that("steps are checked",{
    eemlist <- add_metadata(metadata, example_eems)
    eemlist <- raman_normalize(eemlist)
    qaqc_dir <- system.file("extdata", package = "eemanalyzeR")

    expect_warning(expect_error(get_indices(eemlist, example_abs, qaqc_dir = qaqc_dir), "is_blank_corrected"),"Data has not been fully processed" )
  })

  test_that("flags are correct", {
    qaqc_dir <- system.file("extdata", package = "eemanalyzeR")
    eemlist <- eemR::eem_cut(example_processed_eems, ex=500:900, em=500:900, exact=F) #cut down so there's missing wavelengths
    abslist <- example_processed_abs

    indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir)

    #ensuring flags aren't duplicated
    flags <- stringr::str_split(indices$eem_index$QAQC_flag, "_")
    duplicated <- sapply(flags, function(x){
        if(length(unique(x)) != length(x)){
          return(FALSE)
        }else{return(TRUE)}})

    expect_true(all(duplicated))

  })

  test_that("specific flags are applied", {
    qaqc_dir <- system.file("extdata", package = "eemanalyzeR")
    eemlist <- example_processed_eems
    abslist <- example_processed_abs

    #check for flag on missing DOC
      doc_index <- function(eemlist, abslist, cuvle=1, qaqc_dir=tempdir()){
        return(list(abs_index = data.frame(sample_name="test_samp",
                                           meta_name="test_samp",
                                           index="DOC_test",
                                           value=NA), eem_index=NA))}
      indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir, index_method = doc_index)
      expect_equal(indices$abs_index$QAQC_flag, "DOC01")

    #check for flag on negative values
      neg_index <- function(eemlist, abslist, cuvle=1, qaqc_dir=tempdir()){
        return(list(abs_index = data.frame(sample_name="test_samp",
                                           meta_name="test_samp",
                                           index="test",
                                           value=-1), eem_index=NA))}
      indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir, index_method = neg_index)
      expect_equal(indices$abs_index$QAQC_flag, "NEG01")

    #check for flag on infinite
      inf_index <- function(eemlist, abslist, cuvle=1, qaqc_dir=tempdir()){
        return(list(abs_index = data.frame(sample_name="test_samp",
                                           meta_name="test_samp",
                                           index="test",
                                           value=Inf), eem_index=NA))}
      indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir, index_method = inf_index)
      expect_equal(indices$abs_index$QAQC_flag, "INF01")

    #check for flag on tea standard
      abslist[[2]]$data[,2] <- rep(1,  abslist[[1]]$n)
      indices <- get_indices(eemlist, abslist, qaqc_dir=qaqc_dir)
      expect_equal(sum(grepl("STD01", indices$abs_index$QAQC_flag)), 3)

    #check for flagged values outside the normal range
      expect_equal(sum(grepl("VAL01", indices$abs_index$QAQC_flag)), 2)


  })
