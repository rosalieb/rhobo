#' @title Visualize correction factors at the beginning and end of deployment
#' 
#' @description ggplot visual of correction factors, per lake
#' 
#' @author Rosalie Bruel
#' 
#' @export
#' @param x output dataframe from the correction.factor.do function
#' @keywords hobo
#' @keywords planaqua

pCF <- function(x = NULL) { 
    library(tidyverse)
    
    ggplot(x) + 
        # Add calibrated data
        geom_hline(yintercept = 1, lty = 2) +
        geom_segment(x %>% 
                         select(Lake, What, TC_1, DO_correction_factor, Date_processing) %>%
                         pivot_wider(id_cols = c(Lake, Date_processing), names_from  = What, values_from = c(TC_1, DO_correction_factor)),
                     mapping = aes(x = TC_1_Init, xend = TC_1_End, y = DO_correction_factor_Init, yend = DO_correction_factor_End),
                     lty = 3, col = grey(.4)
        ) +
        geom_point(aes(TC_1, DO_correction_factor, col = Lake), show.legend = FALSE,alpha = .4) +
        # Add manual points (if any is non-NA)
        geom_segment(x %>% 
                         mutate(DO_correction_factor_manual = 
                                    DO_correction_factor_manual %>% 
                                    is.na %>%
                                    ifelse(DO_correction_factor, DO_correction_factor_manual) ) %>%
                         select(Lake, What, TC_1, DO_correction_factor_manual, Date_processing) %>%
                         pivot_wider(id_cols = c(Lake, Date_processing), names_from  = What, values_from = c(TC_1, DO_correction_factor_manual)),
                     mapping = aes(x = TC_1_Init, xend = TC_1_End, y = DO_correction_factor_manual_Init, yend = DO_correction_factor_manual_End)
        ) +
        geom_point(x %>% 
                       mutate(DO_correction_factor_manual = 
                                  DO_correction_factor_manual %>% 
                                  is.na %>%
                                  ifelse(DO_correction_factor, DO_correction_factor_manual) ),
                   mapping = aes(TC_1, DO_correction_factor_manual, col = Lake), show.legend = FALSE, size = 2) +
        facet_wrap(~Lake) +
        labs(x = "Time", y = "DO correction factor",subtitle = paste0("Correction factor calculated on ", paste(unique(x$Date_processing), sep = "", collapse = ", "))) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
}
