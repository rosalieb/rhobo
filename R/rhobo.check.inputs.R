#' @title Check inputs before HOBO QC
#' 
#' @description Take as input all the parameters and does some basic checks (chronological order for dates, files are present in the folder, pressure is in millibar)
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param path2data User's path to the folder where the data are stored. Within that folder, there should be two subfolders with the raw data (Hobo_Raw) and the processed data (Hobo_Process)
#' @param filename path to file we need to calculate the correction factors for. The file is a raw HOBO file, with temperature and oxygen.
#' @param TC_pre datetime of end of the "initialization" calibration (pre-deployment)
#' @param TC_post datetime of end of the "end of deployement" calibration (post-deployment)
#' @param TM_init Time measurement initialization : when probes are back in the lakes
#' @param TM_end Time measurement end: when probes are taken out of the lakes
#' @param Pbaro_mbar_init Atmospheric pressure in millibars during the "initialization" calibration (pre-deployment)
#' @param Pbaro_mbar_end Atmospheric pressure in millibars during the "end of deployement" calibration (post-deployment)
#' @keywords planaqua
#' @keywords hobo

rhobo.check.inputs <- function(path2data, foldername, TC_pre, TC_post, TM_init, TM_end, Pbaro_mbar_init, Pbaro_mbar_end) {
  # 1- Check that the folder has data ####
  files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = FALSE)
  # Give an error message if no files were returned
  if(length(files_raw) == 0) stop(paste0("No raw files were found.\n1) Verify your path: \n   ", paste0(path2data, "/Hobo_Raw/", foldername),"\n   The '/Hobo_Raw/' part is automatically filled, and the folder name ",foldername,"\n   is an argument of the function. \n   If issue here, edit the first part of the function, i.e., the argument 'path2data'.",
                                         "\n2) Verify that the files in the folder have a .txt extension (default).\n   Their name should follow the format of 'lacxx_YYYYMMDD'\n   (xx being the lake number, e.g., 01, and YYYYMMDD the date).\n _____________________"))
  message(paste0(length(files_raw), " files were found in the directory.\n _____________________"))
  
  # 2- Check lake names (must be 01, 02, 03, ..., 16) ####
  lake_index_raw <- unlist(lapply(strsplit(files_raw,"_"), head, 1))
  message(paste0("Lake names are: ", paste(lake_index_raw, collapse = ", "), ".\nCheck they look good (they should be something like: '01', '01L').\n _____________________"))
  
  # 3- Check chronology ####
  temp <- c(TC_pre, TM_init, TM_end,  TC_post)
  temp <- as.POSIXct(temp, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
  if(paste(order(temp), collapse = "") != "1234") message("Problem: the time you entered for TC_pre, TM_init, TM_end and TC_post are not chronologically ordered. Check.\n _____________________")
  
  # 4- Check Pbaro ####
  if(Pbaro_mbar_init < 995) message("Are you sure you entered Pbaro_mbar_init in millibars?\n _____________________")
  if(Pbaro_mbar_end < 995) message("Are you sure you entered Pbaro_mbar_end in millibars?\n _____________________")
}