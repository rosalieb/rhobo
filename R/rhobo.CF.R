#' @title Routine HOBO part 1: compute DO correction factors
#' 
#' @description First part of the QC routine: read in the raw file and compute dissolved oxygen correction factors.
#' 
#' @author Rosalie Bruel and Sophie Guillon
#' 
#' @export
#' @param path2data User's path to the folder where the data are stored. Within that folder, there should be two subfolders with the raw data (Hobo_Raw) and the processed data (Hobo_Process)
#' @param foldername Folder name of the newest data that have been downloaded post-HOBO deployment. Folder name should be the date of the end of the deployment with the format YYYY_MM_DD, e.g., 2021_07_27
#' @param TC_pre datetime of end of the "initialization" calibration (pre-deployment)
#' @param TC_post datetime of end of the "end of deployement" calibration (post-deployment)
#' @param Pbaro_mbar_init Atmospheric pressure in millibars during the "initialization" calibration (pre-deployment)
#' @param Pbaro_mbar_end Atmospheric pressure in millibars during the "end of deployement" calibration (post-deployment)
#' @param n number of observations PRIOR to TC_pre and TC_post used to compute the oxygen mean for computing correction factor. Default to n = 4.
#' @keywords planaqua
#' @keywords hobo

rhobo.CF <- function(path2data, foldername, TC_pre, TC_post, 
                       Pbaro_mbar_init, Pbaro_mbar_end, n = 4) {
  
  library(dplyr)
  
  Sys.setenv(TZ = "GMT")
  
  # 1- List in the files and extract name ####
  files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = FALSE)
  # Extract lake number (01, 02, 03, ..., 16)
  lake_index_raw <- unlist(lapply(strsplit(files_raw,"_"), head, 1))
  # Get file path
  files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = TRUE)
  
  # 2- Read in raw data and calculate the correction factors ####
  read_dat_raw <- vector(mode = "list", length = length(files_raw))
  
  # Now, run function for all element in the table
  for (filename in files_raw) {
    read_dat_raw[[filename]] <- correction.factor.do(filename, TC_pre, TC_post, 
                                                     Pbaro_mbar_init, Pbaro_mbar_end, n)
  }
  
  
  # Bind files together
  metadata_QAQC <- bind_rows(read_dat_raw) 
  # Create the manual DO correction factor. We will add values in that column when the DO factor seems off.
  metadata_QAQC$DO_correction_factor_manual <- NA
  
  return(metadata_QAQC)
}
