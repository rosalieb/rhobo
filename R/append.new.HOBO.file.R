#' @title Combine previous HOBO file with new HOBO file
#' 
#' @description Takes the previously QC'd HOBO file and append the new dataset.
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param newfile data frame of new HOBO dataset that need to be appended to the old one
#' @param file2append data frame of old HOBO dataset the new HOBO data frame will be appended to.
#' @param ind_init index of when to start correcting the measurements
#' @param ind_end index of when to stop correcting the measurements
#' @param CF_init correction factor at the beginning
#' @param TM_init Time measurement initialization : when probes are back in the lakes
#' @param CF_end correction factor at the end
#' @param TM_end Time measurement end: when probes are taken out of the lakes
#' @keywords planaqua
#' @keywords hobo

append.new.HOBO.file <- function(newfile,file2append,CF_init=1,TM_init,CF_end=1,TM_end){
  
  tmplac <- newfile
  file2append$datetime <- as.POSIXct(file2append$datetime, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
  
  ind_init=which(tmplac$datetime==TM_init)
  ind_end=which(tmplac$datetime==TM_end)  
  tmplac[,c('p')] <- NA
  tmplac=interp.p(tmplac,ind_init,ind_end,CF_init,CF_end)
  tmplac$do.obs=tmplac$do.raw*tmplac$p
  
  file2append <- rbind(file2append,tmplac[ind_init:ind_end,])
  
  return(file2append) 
}
