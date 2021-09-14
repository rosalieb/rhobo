#' @title Assign treatments
#' 
#' @description Create a new column for any data frame with lake treatments, based on lake names. Order the lakes by name (default) or treatment
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param x Input dataset
#' @param lakename Character string indicating the name of the column with the lake name information in the x dataset, or alternatively, the column index.
#' @param orderby Choose one of c("number", "treatment", "quadrat", "own") to select the order of lakes. Default: "number". 
#' @param order Default = NULL, or must be a numeric vector with 16 numbers, specified by the user to choose an order, e.g., c(7,8,1:4, 6, 5, 9:16).
#' @keywords PLANAQUA
#' @keywords hobo

rhobo.treatments <- function(x, lakename = NULL, orderby = "number", order = NULL) {
  library(tidyverse)
  
  # Get treatments:
  treatments <- structure(list(LAKENUMBER_TO_USE = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                        13, 14, 15, 16), treatment = structure(c(4L, 2L, 3L, 1L, 1L, 
                                                                                 3L, 2L, 4L, 3L, 1L, 4L, 2L, 2L, 4L, 1L, 3L), .Label = c("Perch - Control", 
                                                                                                                                         "No perch - Control", "Perch - Phosphorus", "No perch - Phosphorus"
                                                                                 ), class = "factor")), row.names = c(NA, -16L), class = "data.frame")
  
  # Remove column treatment if already exists
  if("treatment" %in% colnames(x)) {
    cat(paste0(" There was already a column 'treatment'. It was removed before adding it\n again with the order you chose, i.e., order by '", orderby, "'."))
    x <- x %>% select(-treatment)
    }
  
  # Save column name order to preserve the feature of the dataset:
  varname <- colnames(x)
  
  # Error message to help debug
  if(!orderby %in% c("number", "treatment", "quadrat", "own")| length(orderby)>1) stop(" Choose only one way to 'orderby' among c('number', 'treatment', 'quadrat', 'own').")
  if(is.null(lakename)) stop(" Give the name or the index of the column with the lake name information. ")
  
  # Assign treatment in a new column ####
  x$LAKENUMBER_TO_USE <- as.numeric(gsub("[^0-9.-]", "", x[, lakename]))
  
  x <- merge(x, treatments, by = "LAKENUMBER_TO_USE")
  
  # Transform lake name following the order chosen ####
  if(orderby == "number") {
    orderlakes <- 1:16
    orderlakes <- x %>% mutate(LAKENUMBER_TO_USE = factor(LAKENUMBER_TO_USE, levels = orderlakes)) %>% arrange(LAKENUMBER_TO_USE) %>% pull(var = lakename) %>% unique()
    x[, lakename] <- factor(x[, lakename], levels = orderlakes)
  }
  
  if(orderby == "treatment") {
    orderlakes <- treatments %>% arrange(treatment) %>% pull(LAKENUMBER_TO_USE)
    orderlakes <- x %>% mutate(LAKENUMBER_TO_USE = factor(LAKENUMBER_TO_USE, levels = orderlakes)) %>% arrange(LAKENUMBER_TO_USE) %>% pull(var = lakename) %>% unique()
    x[, lakename] <- factor(x[, lakename], levels = orderlakes)
  }
  
  if(orderby == "quadrat") {
    # Order used for FOAMZ (one type of treatment by corner)
    # 1, 8   |  3, 6           Nutrients - |  Nutrients -
    # 11, 14 |  9, 16          no perch    |    perch
    # -- --- --- -- --       -- --- --- -- -- - -- --- --    
    # 2, 7   |  4, 5          Control - no |  Control -
    # 12, 13 | 10, 15            perch     |    perch
    orderlakes <- c(1,8,3,6,11,14,9,16,2,7,4,5,12,13,10,15)
    orderlakes <- x %>% mutate(LAKENUMBER_TO_USE = factor(LAKENUMBER_TO_USE, levels = orderlakes)) %>% arrange(LAKENUMBER_TO_USE) %>% pull(var = lakename) %>% unique()
    x[, lakename] <- factor(x[, lakename], levels = orderlakes)
  }
  
  if(orderby == "own") {
    orderlakes <- order
    orderlakes <- x %>% mutate(LAKENUMBER_TO_USE = factor(LAKENUMBER_TO_USE, levels = orderlakes)) %>% arrange(LAKENUMBER_TO_USE) %>% pull(var = lakename) %>% unique()
    x[, lakename] <- factor(x[, lakename], levels = orderlakes)
  }
  
  # END: Return object ####
  x <- x %>% select(c(varname, "treatment"))
  return(x)
}
