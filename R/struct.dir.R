#' @title Help showing the structure of the directory
#' 
#' @description Print in the console the folder structure were all the data needed to run the rhobo routine should be.
#'
#' @author 
#'
#' @export
#' @keywords planaqua
#' @keywords hobo
#' @example struct.dir()

struct.dir <- function() {
  if(exists("path2data")) mpath2data = paste0(": \n    ",path2data) else mpath2data = "."
  if(exists("foldername")) mfoldername = foldername else mfoldername = "2021_07_27 (example name)"
  
  cat(paste0(
    " Structure of the directory: \n ('...' below refers to the path2data you gave in the 'path2data' argument", mpath2data,
    ")\n\n .../PLANAQUA",
    "\n        |",
    "\n        |___ HOBO_Process",
    "\n        |       |",
    "\n        |       |___ old",
    "\n        |       |     |", 
    "\n        |       |     |___ [all the previous file (.txt)]",
    "\n        |       |",  
    "\n        |       |___ [most-recent file for each lake]",
    "\n        |",
    "\n        |___ HOBO_Raw",
    "\n                |",
    "\n                |___ ", mfoldername,
    "\n                |     |", 
    "\n                |     |___ [one file per lake (.txt)]",
    "\n                |", 
    "\n                |___ [one folder per field mission]\n"))
  
}
