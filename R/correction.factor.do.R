#' @title Compute dissolved oxygen correction factors
#' 
#' @description Compute dissolved oxygen correction factor pre- and post-deployment by calculating the ratio between the theoritical dissolved oxygen concentration at saturation with the measured oxygen concentration. Requires the atmospheric pressure and the time the calculation can be made for (i.e., when the sensors are taken out their bubble bath, where the oxygen saturation should be at the maximum).  
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param filename path to file we need to calculate the correction factors for. The file is a raw HOBO file, with temperature and oxygen.
#' @param TC_pre datetime of end of the "initialization" calibration (pre-deployment)
#' @param TC_post datetime of end of the "end of deployement" calibration (post-deployment)
#' @param Pbaro_mbar_init Atmospheric pressure in millibars during the "initialization" calibration (pre-deployment)
#' @param Pbaro_mbar_end Atmospheric pressure in millibars during the "end of deployement" calibration (post-deployment)
#' @param n number of observations PRIOR to TC_pre and TC_post used to compute the oxygen mean for computing correction factor. Default to n = 4.
correction.factor.do <- function(filename, TC_pre, TC_post, Pbaro_mbar_init, Pbaro_mbar_end, n = 4) {
  # 0- Initiate function
  # Make sure everything is at the right format
  TC_pre <- parse_date_time(x = TC_pre,
                            orders = c("%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S"), tz = "GMT")
  TC_post  <- parse_date_time(x = TC_post,
                              orders = c("%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S"), tz = "GMT")
  
  # 1- Read in file
  tmplac <- read.hobo(filename)  
  lakename = unlist(lapply(strsplit(unlist(lapply(strsplit(filename, "/"), tail, 1)), "_"), head, 1))
  
  # 2- Subset init and end dataset
  # Find here the index of the closest sample matching our condition. Allows for potential approximation.
  index = (tmplac$datetime- TC_pre)[(tmplac$datetime- TC_pre)<=0]
  index = which(index == max(index))
  tmpinit <- tmplac[index:(index+1-n), ]
  # Same for end
  index =(tmplac$datetime- TC_post)[(tmplac$datetime- TC_post)<=0]
  index = which(index == max(index))
  tmpend <- tmplac[index:(index+1-n), ]
  
  # 3- Calculate DO sat
  tmpinit$do.sat <- o2.at.sat(ts.data = tmpinit[, c("datetime", "wtr")], baro = Pbaro_mbar_init)$do.sat
  tmpend$do.sat <- o2.at.sat(ts.data = tmpend[, c("datetime", "wtr")], baro = Pbaro_mbar_end)$do.sat
  
  # 4- Compute
  Correction_factor_init <- mean(tmpinit$do.sat, na.rm = T) / mean(tmpinit$do.raw, na.rm = T) 
  Correction_factor_end <- mean(tmpend$do.sat, na.rm = T) / mean(tmpend$do.raw, na.rm = T) 
  
  # 5- Save output
  out <- data.frame(
    "Lake" = lakename,
    "What" = c("Init", "End"),
    "T_user" = c(TC_pre, TC_post),
    "TC_1" = c(tmpinit$datetime[n], tmpend$datetime[n]),
    "TC_n" = c(tmpinit$datetime[1], tmpend$datetime[1]),
    "Pbaro_millibar" = c(Pbaro_mbar_init, Pbaro_mbar_end),
    "DO_sat_mean" = c(mean(tmpinit$do.sat, na.rm = T), mean(tmpend$do.sat, na.rm = T)),
    "DO_HOBO_mean" = c(mean(tmpinit$do.raw, na.rm = T), mean(tmpend$do.raw, na.rm = T)),
    "DO_correction_factor" = c(Correction_factor_init, Correction_factor_end),
    "Date_processing" = Sys.Date())
  
  return(out)
  
}
