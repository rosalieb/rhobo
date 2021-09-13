#' @title Read in a HOBO file
#' 
#' @description The function read in a hobo file in the format it is exported as at the CEREEP-Ecotron, i.e., as a .csv file. The columns are renamed and the date time format is converted to a date time format R understands.
#' 
#' @author Rosalie Bruel and Sophie Guillon
#' 
#' @export
#' @param filename path to HOBO dataset to read
#' @keywords hobo 
#' @keywords planaqua

read.hobo <- function(filename){
  tmp <- read.csv(filename,header = FALSE, skip = 2, sep = "\t", dec=".", stringsAsFactors = FALSE)
  tmplac <- tmp[,1:4]
  colnames(tmplac) <- c("lac","datetime","do.raw","wtr")
  # Careful, column "lac" is not filled with correct value. 
  # Use the code in the filename
  tmplac$lac <- unlist(lapply(strsplit(unlist(lapply(strsplit(filename,"/"), tail, 1)),"_"), head, 1))
  tmplac$datetime<- as.POSIXct(tmplac$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
  
  return(tmplac)
}