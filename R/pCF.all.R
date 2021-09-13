#' @title Visualize correction factors at the beginning and end of deployment
#' 
#' @description ggplot visual of correction factors, per lake. Uses 
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param x output dataframe from the correction.factor.do function
#' @param alldat logical argument, default = TRUE. Whether to include all the previous metadata file or not
#' @param lakename Character string indicating the name of the column with the lake name information in the x dataset, or alternatively, the column index.
#' @param orderby Choose one of c("number", "treatment", "quadrat", "own") to select the order of lakes. Default: "number". 
#' @param order Default = NULL, or must be a numeric vector with 16 numbers, specified by the user to choose an order, e.g., c(7,8,1:4, 6, 5, 9:16).
#' @param xmin Minimum x you want to see. Format = "YYYY-MM-DD" 
#' @param xmax Maximum x you want to see. Format = "YYYY-MM-DD"
#' @keywords hobo
#' @keywords planaqua

pCF.all <- function(x = NULL, alldat = TRUE, lakename = "Lake", orderby = "number", order = NULL, xmin = NULL, xmax = NULL) {
  # 1- Do some QAQC ####
  if(is.null(x) & alldat == FALSE) {
    cat(" You need to specify either a input data or set the alldat argument to TRUE. \n alldat was automatically set to TRUE to be able to visualize something.")
    alldat = TRUE
  }
  
  # Set xmin and xmax as dates
  if(!is.null(xmin)) xmin <- parse_date_time(x = TM_init, orders = c("%Y/%m/%d","%Y-%m-%d"), tz = "GMT")
  if(!is.null(xmax)) xmax <- parse_date_time(x = TM_init, orders = c("%Y/%m/%d","%Y-%m-%d"), tz = "GMT")
  
  # Require lakename. Works with the standard format of metadata wherelakename == "Lake"
  
  # 2- read in the additional data if alldat == TRUE ####
  if(alldat) {
    whichfolder <- list.files(paste0(path2data, "/Hobo_Raw"))
    x2 <- NULL
    for(i in whichfolder) {
      # ID the most recent file:
      files <- list.files(paste0(path2data, "/Hobo_Raw/", i), pattern = c("DO_Correction_factor"))
      files <- files[grep(".csv", files)]
      date_extract <- as.Date(str_replace(unlist(lapply(strsplit(files, "_"),tail,1)), ".csv", ""))
      files <- files[max(order(date_extract))]
      
      # Read the file and append:
      if(!is.na(files)) {
        temp <- read.csv(paste0(path2data, "/Hobo_Raw/", i, "/", files))
        x2<- bind_rows(x2, temp)
      }
    }
    x2$T_user <- as.POSIXct(x2$T_user, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    x2$TC_1   <- as.POSIXct(x2$TC_1, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    x2$TC_n   <- as.POSIXct(x2$TC_n, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
  }
  
  # 3- Merge x and x2 if !is.null(x) & alldat == TRUE
  if(!is.null(x) && alldat == TRUE) {
    x <- bind_rows(x, x2)
  }
  
  # 4- Add treatments
  x <- rhobo.treatments(x, lakename = lakename, orderby = orderby, order = order)
  
  if(is.null(xmin) & !is.null(xmax)) xmin = as.Date(min(x$TC_1), format = "%Y-%m-%d")
  if(!is.null(xmin) & is.null(xmax)) xmax = as.Date(max(x$TC_1), format = "%Y-%m-%d")
  
  if(all(!is.null(c(xmin, xmax)))) pCF(x) + lims(xmin, xmax) else pCF(x)
}