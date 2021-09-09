#' @title Routine HOBO part 2: Apply correction factors and append to previous files
#' 
#' @description Apply the correction factors to measured dissolved oxygen and append file for each lake to previous file. If no previous file was found, only the new file is kept.
#' 
#' @author Rosalie Bruel 
#' @author Sophie Guillon
#' 
#' @export
#' @param metadata_QAQC output from the rhobo.CF() function. Data frame with the correction factors for all the new HOBO files.
#' @param path2data User's path to the folder where the data are stored. Within that folder, there should be two subfolders with the raw data (Hobo_Raw) and the processed data (Hobo_Process)
#' @param foldername Folder name of the newest data that have been downloaded post-HOBO deployment. Folder name should be the date of the end of the deployment with the format YYYY_MM_DD, e.g., 2021_07_27
#' @param TM_init Time measurement initialization : when probes are back in the lakes
#' @param TM_end Time measurement end: when probes are taken out of the lakes
#' @keywords planaqua
#' @keywords hobo

rhobo.append <- function(metadata_QAQC, path2data, foldername, TM_init, TM_end) {
  
  Sys.setenv(TZ = "GMT")
  
  # 1.1- create the Append text that will be added to the output files.
  Append = format(as.Date(foldername, format = "%Y_%m_%d"), format = "%Y%m") # To respect the format, create the append that will be add to each QAQC'd file
  
  # 1.2- Path to new files (raw): List in the files and extract name
  files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = FALSE)
  # Give an error message if no files were returned
  if(length(files_raw) == 0) stop(paste0("No raw files were found.\n1) Verify your path: \n   ", paste0(path2data, "/Hobo_Raw/", foldername),"\n   The '/Hobo_Raw/' part is automatically filled, and the folder name ",foldername,"\n   is an argument of the function. \n   If issue here, edit the first part of the function, i.e., the argument 'path2data'.",
                                         "\n2) Verify that the files in the folder have a .txt extension (default).\n   Their name should follow the format of 'lacxx_YYYYMMDD'\n   (xx being the lake number, e.g., 01, and YYYYMMDD the date)."))
  # Extract lake number (01, 02, 03, ..., 16)
  lake_index_raw <- unlist(lapply(strsplit(files_raw,"_"), head, 1))
  # Get file path
  files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = TRUE)
  
  # 1.3- Read in the previous compiled files
  files_proccess <- list.files(paste0(path2data, "/Hobo_Process"), pattern = ".txt", full.names = FALSE)
  
  # Extract lake number (01, 02, 03, ..., 16)
  lake_index <- unlist(lapply(strsplit(files_proccess,"_"), head, 1))
  # Extract date
  date_index <- as.Date(paste0(substr(unlist(lapply(strsplit(files_proccess,"_"), tail, 1)), 1,6), "01"), format = "%Y%m%d")
  
  # select only one file per lake (lake_index vector), and within lake, select only the most recent file (date_index vector)
  which2keep = NULL
  for (i in unique(lake_index)) {
    which2keep <- c(which2keep, which(lake_index == i)[order(date_index[which(lake_index == i)], decreasing = TRUE)[1]])
  }
  lake_index_process <- lake_index[which2keep]
  
  files_proccess <- list.files(paste0(path2data, "/Hobo_Process"), pattern = ".txt", full.names = TRUE)
  # This vector below will have path to only the most recent files.
  files_proccess <- files_proccess[which2keep]
  
  # Read in data
  read_dat_process <- vector(mode = "list", length = length(files_raw))
  
  # Correction factor data frame
  CF_dt <- metadata_QAQC %>% 
    mutate(DO_CF = 
             DO_correction_factor_manual %>% 
             is.na %>%
             ifelse(DO_correction_factor, DO_correction_factor_manual)) %>% 
    select(Lake, What, DO_CF)
  
  # For each 'file_raw' (each .txt files downloaded with the HOBO software), readin and append.
  for (i in 1:length(files_raw)) {
    # Read in new file
    newfile <- read.hobo(files_raw[i])
    newfile$lac <- substr(lake_index_process[i], 4, nchar(lake_index_process))
    if(!is.na(as.numeric(paste(newfile$lac[1])))) newfile$lac <- as.numeric(paste(newfile$lac))
    
    # Read in previous corrected file = file to append
    if(! lake_index_raw[i] %in% lake_index_process) stop("Couldn't find the file to append to. Lake names are different.")
    tmppath <- files_proccess[which(lake_index_process == lake_index_raw[i])]
    file2append <- read.delim(tmppath)
    outpath <- substr(tmppath, 1, nchar(tmppath)-10)
    
    outfile <- append.new.HOBO.file(newfile = newfile,file2append = file2append,
                                    CF_init = CF_dt$DO_CF[CF_dt$Lake == lake_index_raw[i] & CF_dt$What == "Init" ],
                                    CF_end = CF_dt$DO_CF[CF_dt$Lake == lake_index_raw[i] & CF_dt$What == "End"],
                                    TM_init = TM_init,
                                    TM_end = TM_end)
    outfile$lac <- as.character(paste(outfile$lac))
    
    read_dat_process[[i]]<- outfile
    if(file.exists(paste0(outpath, Append, ".txt"))) stop(paste0("A file with this name (",outpath,Append,".txt) already exist in the folder. Delete it first if you want to overide it."))
    write.table(outfile, file = paste0(outpath, Append, ".txt") , append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
    
    # Return list with all the data frame
    return(read_dat_process)
  }
  
}
