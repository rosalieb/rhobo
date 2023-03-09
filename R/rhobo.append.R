#' @title Routine HOBO part 2: Apply correction factors and append to previous files
#' 
#' @description Apply the correction factors to measured dissolved oxygen and append file for each lake to previous file. If no previous file was found, only the new file is kept.
#' 
#' @author Rosalie Bruel and Sophie Guillon
#' 
#' @export
#' @param metadata_QAQC output from the rhobo.CF() function. Data frame with the correction factors for all the new HOBO files.
#' @param path2data User's path to the folder where the data are stored. Within that folder, there should be two subfolders with the raw data (Hobo_Raw) and the processed data (Hobo_Process)
#' @param foldername Folder name of the newest data that have been downloaded post-HOBO deployment. Folder name should be the date of the end of the deployment with the format YYYY_MM_DD, e.g., 2021_07_27
#' @param TM_init Time measurement initialization : when probes are back in the lakes
#' @param TM_end Time measurement end: when probes are taken out of the lakes
#' @param write Whether to save the output file or not. Logical.
#' @keywords planaqua
#' @keywords hobo

rhobo.append <- function(metadata_QAQC, 
                         path2data, 
                         foldername, 
                         TM_init, TM_end, 
                         write = TRUE) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  
  Sys.setenv(TZ = "GMT")
  
  # 1- Pre computation ####
  message(" Initialization.......... \n")
  
  # 1.1- Check inputs ####
  # Change the format of TM_init and TM_end
  TM_init <-  parse_date_time(x = TM_init, orders = c("%Y/%m/%d %H:%M:%S","%Y-%m-%d %H:%M:%S"), tz = "GMT")
  TM_end  <-  parse_date_time(x = TM_end, orders = c("%Y/%m/%d %H:%M:%S","%Y-%m-%d %H:%M:%S"), tz = "GMT")

  
  # 1.2- Path to new files (raw): List in the files and extract name ####
  files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = FALSE)
  # Give an error message if no files were returned
  if(length(files_raw) == 0) stop(paste0("No raw files were found.\n1) Verify your path: \n   ", paste0(path2data, "/Hobo_Raw/", foldername),"\n   The '/Hobo_Raw/' part is automatically filled, and the folder name ",foldername,"\n   is an argument of the function. \n   If issue here, edit the first part of the function, i.e., the argument 'path2data'.",
                                         "\n2) Verify that the files in the folder have a .txt extension (default).\n   Their name should follow the format of 'lacxx_YYYYMMDD'\n   (xx being the lake number, e.g., 01, and YYYYMMDD the date)."))
  # Extract lake number (01, 02, 03, ..., 16)
  lake_index_raw <- unlist(lapply(strsplit(files_raw,"_"), head, 1))
  # Get file path
  files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = TRUE)
  
  # 1.3- Read in the previous compiled files ####
  # 1.3.1- Move all the files in process to the "old" folder
  # Create folder "old" if it doesn't exist
  dir.create(paste0(path2data, "/Hobo_Process/old"), showWarnings = FALSE)
  
  # List files
  files_from <- list.files(paste0(path2data, "/Hobo_Process"), pattern = ".txt", full.names = FALSE)
  files_to <- c(paste0(path2data, "/Hobo_Process/old/", files_from))
  # Copy all the files
  lapply(seq_along(files_from), function (x) file.copy(from = paste0(path2data, "/Hobo_Process/",files_from)[x],
            to = files_to[x]))
  # Remove all the files
  lapply(seq_along(files_from), function (x) file.remove(paste0(path2data, "/Hobo_Process/",files_from)[x]))
  
  # 1.3.2- The "old" folder contains all the data for all the lakes. For each lakes, we'll ID the most recent dataset.
  temp <- list.recent.files(paste0(path2data, "/Hobo_Process/old"))
  files_process <- temp$files
  lake_index_process <- temp$index # Naming both is not necessary, but on 14-09-2021, this line is not uploading, and lake_index_process cannot be found. Seeing if this trick solves the problem.
  
  
  #2- Create new file with appended data for each lake ####
  read_dat_process <- vector(mode = "list", length = length(files_raw))
  
  # 2.1- Correction factor data frame ####
  CF_dt <- metadata_QAQC %>% 
    mutate(DO_CF = 
             DO_correction_factor_manual %>% 
             is.na %>%
             ifelse(DO_correction_factor, DO_correction_factor_manual)) %>% 
    select(Lake, What, DO_CF)
  
  # 2.2- Loop for each file starts here ####
  # For each 'file_raw' (each .txt files downloaded with the HOBO software), read-in and append, when corresponding file exist.
  for (i in 1:length(files_raw)) {
    message(paste0(" Starting ", lake_index_raw[i]," (",i,"/",length(files_raw),")..."))
    # Read in new file
    newfile <- read.hobo(files_raw[i])
    newfile$lac <- substr(lake_index_raw[i], 4, nchar(lake_index_raw[i]))
    if(!is.na(as.numeric(paste(newfile$lac[1])))) newfile$lac <- as.numeric(paste(newfile$lac))
    
    # Read in previous corrected file = file to append
    if(lake_index_raw[i] %in% lake_index_process) {
      tmppath <- files_process[which(lake_index_process == lake_index_raw[i])]
      file2append <- read.delim(tmppath)
     
      cat(paste0("     The new file '", unlist(lapply(strsplit(files_raw[i], "/"), tail, 1)),"' will be appended to the \n     file '", unlist(lapply(strsplit(tmppath, "/"), tail, 1)), "'.\n"))
      
      outfile <- append.new.HOBO.file(newfile = newfile,file2append = file2append,
                                      CF_init = CF_dt$DO_CF[CF_dt$Lake == lake_index_raw[i] & CF_dt$What == "Init" ],
                                      CF_end = CF_dt$DO_CF[CF_dt$Lake == lake_index_raw[i] & CF_dt$What == "End"],
                                      TM_init = TM_init,
                                      TM_end = TM_end)
    } else {
    # If no previous file was found, just write the current newdata file alone
      cat(paste0("\n     No corresponding older file was found for the new file \n     '", unlist(lapply(strsplit(files_raw[i], "/"), tail, 1)), ".\n"))
      
      outfile <- append.new.HOBO.file(newfile = newfile,file2append = NULL,
                                      CF_init = CF_dt$DO_CF[CF_dt$Lake == lake_index_raw[i] & CF_dt$What == "Init" ],
                                      CF_end = CF_dt$DO_CF[CF_dt$Lake == lake_index_raw[i] & CF_dt$What == "End"],
                                      TM_init = TM_init,
                                      TM_end = TM_end)  
    }
    
    # Create outpath name
    start_date <- format(as.Date(outfile$datetime[1]), format = "%Y%m")
    end_date <- format(as.Date(tail(outfile$datetime, 1)), format = "%Y%m")
    outpath <- paste0(path2data, "/Hobo_Process/old/",lake_index_raw[i], "_data_compile_", start_date,"_", end_date,".txt")
    
    # Turn lake ID to character
    outfile$lac <- as.character(paste(outfile$lac))
    
    # Check there is only one lake id 
    if(length(unique(outfile$lac))>1) stop(paste0("More than one lake ID was created for ", lake_index_raw[i], ": ", paste(unique(outfile$lac), collapse = ", "),".\n Go to the root files and functions to solve the issue."))
    
    # Write data
    read_dat_process[[i]]<- outfile
    if(write) {
      if(file.exists(outpath)) stop(paste0("A file with this name (",outpath,Append,".txt) already exist in the folder. Delete it first if you want to overide it."))
      write.table(outfile, file = outpath , append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)
      
      # Print progress
      cat(paste0("     ...Done!\n     '", unlist(lapply(strsplit(outpath,split = "/"), tail, 1)), "' was written in the home folder.\n\n"))
    } else {
      # Print progress
      cat(paste0("     ...Done!\n     However, '", unlist(lapply(strsplit(outpath,split = "/"), tail, 1)), "' was not \n     written in the home folder because you chose write = FALSE.\n\n"))
    }
    
        
    
  }
  
  # 3- Move most recent files to process folder ####
  message(" ........Almost done........")
  cat("\n     Now moving files to 'Hobo_Process/old' so that only the most-recent \n     files (one per file ID) will be in the folder 'Hobo_Process'....\n")
  # List files
  files_from <- list.recent.files(paste0(path2data, "/Hobo_Process/old"))$files
  files_to <- str_replace(files_from, "/old", "")
  # Copy all the files
  lapply(seq_along(files_from), function (x) file.copy(from = files_from[x],
                                                       to = files_to[x]))
  # Remove all the files
  lapply(seq_along(files_from), function (x) file.remove(files_from[x]))
  
  message("\n ........................Done!\n\n")
  
  # Return list with all the data frame created in step 2.
  return(read_dat_process)
}
