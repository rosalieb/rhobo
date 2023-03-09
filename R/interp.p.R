#' @title  Interpolate the correction factor
#' 
#' @description Interpolate the correction factor at each time step between the start and end of deployment.
#' 
#' @author Sophie Guillon
#' 
#' @export
#' @param x HOBO dataset
#' @param ind_init index of when to start correcting the measurements (start of deployment)
#' @param ind_end index of when to stop correcting the measurements (end of deployment)
#' @param CF_init correction factor at the beginning
#' @param CF_end correction factor at the end
#' @keywords planaqua
#' @keywords hobo


interp.p <- function(x,ind_init,ind_end,CF_init,CF_end){
  X=(CF_end-CF_init)/as.double.difftime(x$datetime[ind_end]-x$datetime[ind_init],units=c("days"))
  for(i in seq(ind_init,ind_end)){
    x$p[i]=CF_init+ X*as.double.difftime(x$datetime[i]-x$datetime[ind_init],units=c("days"))
  }
  return(x) 
}