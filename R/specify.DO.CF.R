#' @title Specify DO correction factor manually
#' 
#' @description After visual inspection, the user can decide to specify a correction factor manually.
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param x the dataframe with the output
#' @param name name of the lake as character, e.g., "lac01"
#' @param time "Init" or "End"
#' @param CF manual correction factor to enter
#' @keywords hobo
#' @keywords planaqua

specify.DO.CF <- function(x, name, time, CF) {
  if (!"DO_correction_factor_manual" %in% colnames(x)) x$DO_correction_factor_manual <- NA
  x$DO_correction_factor_manual[x$Lake == name & x$What == time] <- CF
  return(x)
}
