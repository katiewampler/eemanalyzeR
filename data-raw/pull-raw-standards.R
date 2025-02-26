path <- "C:/Users/wampleka/Box/FEWS_Box/Lab-work/Aqualog/instrument-exports/"

#search for blank files
#if this takes a while, open the folder and go to properties,
  #I think the files need to be "mapped" or something, wait for the size and number of files to stabilize
files <- list.files(path, recursive = T, pattern=".dat")

  blank_eem <- grep(pattern = "2_Blanks/Blk", files, ignore.case = T, value=T)
    #gets the instrument blank for the sample blank -> this should be the same as the instrument blank for all the samples in each group
  blank_abs <- grep(pattern= "1_Absorbance/Blk", files, ignore.case = T, value=T)

  tea_eem <- grep(pattern = "3_Samples/preTea|3_Samples/postTea", files, ignore.case = T, value=T)
  tea_abs <- grep(pattern= "1_Absorbance/preTea|1_Absorbance/postTea", files, ignore.case = T, value=T)

#save files to raw-data in eemR package
  save_path <- "Z:/13_RPackages/eemanalyzeR/data-raw/long term standards/"
  file.copy(file.path(path, blank_eem), file.path(save_path, "EEM/blanks", basename(blank_eem)))
  file.copy(file.path(path, blank_abs), file.path(save_path, "absorbance/blanks", basename(blank_abs)))
  file.copy(file.path(path, tea_eem), file.path(save_path, "EEM/tea-standards", basename(tea_eem)))
  file.copy(file.path(path, tea_abs), file.path(save_path, "absorbance/tea-standards", basename(tea_abs)))
