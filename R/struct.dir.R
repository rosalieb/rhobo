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
  cat(paste0(
    " Structure of the directory: \n ('...' below refers to the path2data you gave in the 'path2data' argument.",
    ")\n\n .../PLANAQUA",
    "\n        |",
    "\n        |___ HOBO_Process",
    "\n        |       |",
    "\n        |       |___ old",
    "\n        |       |     |", 
    "\n        |       |     |___ [all the previous file]",
    "\n        |       |",  
    "\n        |       |___ [recent-most file for each lake]",
    "\n        |",
    "\n        |___ HOBO_Raw",
    "\n                |",
    "\n                |___ ", foldername,
    "\n                |     |", 
    "\n                |     |___ [one file per lake]",
    "\n                |", 
    "\n                |___ [one folder per field mission]\n"))
  
}
