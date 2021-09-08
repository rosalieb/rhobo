#' Read in a HOBO file
#' 
#' @description The function read in a hobo file in the format it is exported as at the CEREEP-Ecotron, i.e., as a .csv file. The columns are renamed and the date time format is converted to a date time format R understands.
#' 
#' @param filename path to HOBO dataset to read
#' @keywords hobo planaqua
#' @example read.hobo("Data/20210726/lac01.csv")

read.hobo <- function(filename){
  tmp <- read.csv(filename,header = FALSE, skip = 2, sep = "\t", dec=".", stringsAsFactors = FALSE)
  tmplac <- tmp[,1:4]
  colnames(tmplac) <- c("lac","datetime","do.raw","wtr")
  # careful, column "lac" is not filled with correct value. Solution: turn to NA for now:
  tmplac$lac <- NA
  tmplac$datetime<- as.POSIXct(tmplac$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
  
  return(tmplac)
}