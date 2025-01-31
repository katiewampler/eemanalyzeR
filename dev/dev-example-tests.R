# testing functions with real data. these are data sets that previously caused issues in the fewsdom code
  #this data won't be included in the package because:
      #(1) the files are large
      #(2) we don't own most of the data

  #this testing is helpful because:
      #(1) it tests different file naming structures
      #(2) it tests different sizes/shapes of files

  #example 1: Hohner-Lab-2024-07-29
    input_dir <- "~/Katie Coding/eemanalyzeR/dev/dev-examples/Hohner-Lab-2024-07-29"
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)

  #example 2: Hohner-Lab-2025-01-08
    input_dir <- "~/Katie Coding/eemanalyzeR/dev/dev-examples/Hohner-Lab-2025-01-08"
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)

  #example 3: PNNL-2022-11-10
    input_dir <- "~/Katie Coding/eemanalyzeR/dev/dev-examples/PNNL-2022-11-10"
    abs <- abs_dir_read(input_dir) # like this, we get warnings, but still loads
    abs <- abs_dir_read(input_dir, pattern="Abs") #like this we don't get warnings
    eems <- eem_dir_read(input_dir)

  #example 4: Vick-Majors-Lab-2024-11-04
    input_dir <- "~/Katie Coding/eemanalyzeR/dev/dev-examples/Vick-Majors-Lab-2024-11-04"
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
