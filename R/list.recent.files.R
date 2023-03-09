#' @title List most recent files
#' 
#' @description Will find inside the folder with all the files the file with the recent-most data, per lake. Name of each data file sould follow the format: "lacXX_data_compile_YYYYMM_YYYYMM.txt" or "lacXX_data_compile_YYYYMM.txt"
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param path Path to folder with all the files. Format of files within the path should be "lacXX_data_compile_YYYYMM_YYYYMM.txt". lacXX gives the lac number, and YYYYMM are the start and end dates of the recording. The second date will be used to filter out the most recent file. 
#' @keywords planaqua
#' @keywords hobo

list.recent.files <- function(path) {
  # List files
  files_all <- list.files(path, pattern = ".txt", full.names = FALSE)
  # Extract lake number (01, 02, 03, ..., 16)
  index_all <- unlist(lapply(strsplit(files_all,"_"), head, 1))
  # Extract date
  date_index_all <- as.Date(paste0(substr(unlist(lapply(strsplit(files_all,"_"), tail, 1)), 1, 6), "01"), format = "%Y%m%d")
  
  # select only one file per lake (lake_index vector), and within lake, select only the most recent file (date_index vector)
  which2keep = NULL
  for (i in unique(index_all)) {
    which2keep <- c(which2keep, which(index_all == i)[order(date_index_all[which(index_all == i)], decreasing = TRUE)[1]])
  }
  index_all <- index_all[which2keep]
  
  files_all <- list.files(path, pattern = ".txt", full.names = TRUE)
  # This vector below will have path to only the most recent files.
  files_all <- files_all[which2keep]
  
  x <- list(files = files_all, index = index_all)
  return(x)
}
