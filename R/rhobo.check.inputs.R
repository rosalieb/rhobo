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

rhobo.check.inputs <- function(path2data = NULL, foldername = NULL, 
                               TC_pre = NULL, TC_post = NULL, 
                               TM_init = NULL, TM_end = NULL, 
                               Pbaro_mbar_init = NULL, Pbaro_mbar_end = NULL) {
  
  library(lubridate)
  cat("\n Read the messages below to know whether you need to edit some of the\n entry parameters.\n\n _____________________")
  
  
  # 1- Check the structure of path2data, and whether the folder we need are there.
  cat("\n 1. Directory structure\n")
  if(is.null(path2data)) {
    cat("    path2data argument was not specified and therefore not verified.\n _____________________")
  } else {
    PB = FALSE # Default: no problem with the directory structure
    mmessage = NULL
    
    if(any(!c("Hobo_Process","Hobo_Raw") %in% list.files(path = path2data))) {
      PB = TRUE
      mmessage <- c(mmessage, "      - The folder(s) Hobo_Process and/or Hobo_Raw are missing from the directory ", path2data,".\n")
    }
    if(!foldername %in% list.files(path = paste0(path2data, "/Hobo_Raw"))) {
      PB = TRUE
      mmessage <- c(mmessage, "      - The folder ", foldername," is missing from the directory ", path2data, "/Hobo_Raw.\n")
    }
    if(!"old" %in% list.files(path = paste0(path2data, "/Hobo_Process"))) {
      PB = TRUE
      mmessage <- c(mmessage, "      - The folder 'old' is missing from the directory ", path2data,"/Hobo_Raw.\n")
    }
    
    if(PB == TRUE) {
      message(" There is an issue with your directory structure.")
      cat(paste0("\n Identified problems seems to be: \n",paste(mmessage, collapse = "")," \n One or several folders are either missing, misnamed, or misplaced. \n Study the structure below and be sure it matches your folders locally.\n Access the required directory structure at any moment with the function struct.dir(). \n")) 
      struct.dir()
      stop()
    } else {
      cat(paste0("\n _____________________\n     The structure of your directory seems to match the requirements. \n     Access the required directory structure at any moment with the\n     function struct.dir().\n _____________________"))
    }
    
  }
  
  # 2- Check that the folder has data ####
  cat("\n 2. Presence of datasets in raw folder \n")
  if(any(c(is.null(path2data),is.null(foldername)))) {
    cat("    We cannot check the presence of data without specifying the path2data\n and foldername arguments.\n _____________________")
  } else {
    files_raw <- list.files(paste0(path2data, "/Hobo_Raw/", foldername), pattern = ".txt", full.names = FALSE)
    # Give an error message if no files were returned
    if(length(files_raw) == 0) stop(paste0("\n    No raw files were found.\n1) Verify your path: \n   ", paste0(path2data, "/Hobo_Raw/", foldername),"\n   The '/Hobo_Raw/' part is automatically filled, and the folder name ",foldername,"\n   is an argument of the function. \n   If issue here, edit the first part of the function, i.e., the argument 'path2data'.",
                                           "\n2) Verify that the files in the folder have a .txt extension (default).\n   Their name should follow the format of 'lacxx_YYYYMMDD'\n   (xx being the lake number, e.g., 01, and YYYYMMDD the date).\n _____________________"))
    cat(paste0("\n     ",length(files_raw), " files were found in the directory.\n _____________________"))
  }
  
  # 3- Check lake names (must be 01, 02, 03, ..., 16) ####
  cat("\n 3. Checking lake names \n")
  if(any(c(is.null(path2data),is.null(foldername)))) {
    cat("    Lake names cannot be checked without specifying the path2data and\n foldername arguments.\n _____________________")
  } else {
    lake_index_raw <- unlist(lapply(strsplit(files_raw,"_"), head, 1))
    cat(paste0("\n     Lake names are: ", paste(lake_index_raw, collapse = ", "), "."))
    message(paste0("\n     Please make sure the names look good (they should be something like: '01', '01L')."))
    cat(" _____________________")
  }
  
  # 4- Check chronology ####
  cat("\n 4. Checking chronology in your input parameters \n")
  if(any(c(is.null(TC_pre), is.null(TM_init), is.null(TM_end), is.null(TC_post)))) {
    if(all(c(is.null(TC_pre), is.null(TM_init), is.null(TM_end), is.null(TC_post)))){
      cat("    TC_pre, TM_init, TM_end, and TC_post were not specified and\n therefore not verified.\n _____________________")
    } else {
      message("   Please specify all of TC_pre, TM_init, TM_end, and TC_post arguments.")
    }
  } else {
    temp <- c(TC_pre, TM_init, TM_end, TC_post)
    temp <- parse_date_time(x = temp,
                            orders = c("%Y/%m/%d %H:%M:%S","%Y-%m-%d %H:%M:%S"), tz = "GMT")
    if(paste(order(temp), collapse = "") != "1234") message("\n    Problem: the time you entered for TC_pre, TM_init, TM_end and TC_post are not chronologically ordered. Check.\n _____________________") else cat("\n    Looks ok.\n _____________________")
  }
  
  # 5- Check Pbaro ####
  cat("\n 5. Checking atmospheric pressure \n")
  if(any(c(is.null(Pbaro_mbar_init), is.null(Pbaro_mbar_end)))) {
    if(all(c(is.null(Pbaro_mbar_init), is.null(Pbaro_mbar_end)))) {
      cat("    Pbaro_mbar_init and Pbaro_mbar_end were not specified and\n therefore not verified.\n _____________________")
    } else {
      message("    Please specify both Pbaro_mbar_init and Pbaro_mbar_end.")
    }
  } else {
    if(Pbaro_mbar_init < 995) message("     Are you sure you entered Pbaro_mbar_init in millibars?\n _____________________")
    if(Pbaro_mbar_end < 995) message("     Are you sure you entered Pbaro_mbar_end in millibars?\n _____________________")
    if(all(c(Pbaro_mbar_init,Pbaro_mbar_end) >= 995)) cat("\n     Looks ok.\n _____________________")
  } 
  
  # 6. Done!
  if(all(is.null(c(path2data, foldername, TC_pre, TM_init, TM_end, TC_post, Pbaro_mbar_init, Pbaro_mbar_end))))
    message("\n........No entry parameter- specify at least some of the arguments!") else 
      message("\n....................... Done!")
}
