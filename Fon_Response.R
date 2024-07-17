library(tidyverse)
library(magrittr)

Select <- function(dta,prPreU,prPreL,prPostU,prPostL){
  
  # treated group in pre treatment period
  predta <- dta %>% filter(G > period)
  predta <- tidyr::pivot_wider(dta %>% filter(G > period),
                               names_from = period,
                               values_from = Y,
                               names_glue = "{.value}{period}")
  # units with positive FE
  predtaU <- predta %>% 
    filter(X >= 0) %>% 
    do(sample_n(.,prPreU*nrow(.)))
  # units with negative FE
  predtaL <- predta %>% 
    filter(X < 0) %>% 
    do(sample_n(.,prPreL*nrow(.)))
  # combine treated group in pretreatment period 
  predta_sel <- tidyr::pivot_longer(rbind.data.frame(predtaU, predtaL),
                                    cols=tidyr::starts_with("Y"),
                                    names_to="period",
                                    names_prefix="Y",
                                    values_to="Y",
                                    values_drop_na = TRUE)
  
  
  # treated group in posttreatment period, note that we remove untreated group
  postdta <- tidyr::pivot_wider(dta %>% filter(G <= period & !G==0),
                                names_from = period,
                                values_from = Y,
                                names_glue = "{.value}{period}")
  
  # units with positive FE
  postdtaU <- postdta %>% 
    filter(X >= 0) %>% 
    do(sample_n(.,prPostU*nrow(.)))
  # units with negative FE
  postdtaL <- postdta %>% 
    filter(X < 0) %>% 
    do(sample_n(.,prPostL*nrow(.)))
  # combine treated group in pretreatment period 
  postdta_sel <- tidyr::pivot_longer(rbind.data.frame(postdtaU, postdtaL),
                                     cols=tidyr::starts_with("Y"),
                                     names_to="period",
                                     names_prefix="Y",
                                     values_to="Y",
                                     values_drop_na = TRUE)
  
  
  ddf <- rbind.data.frame(predta_sel, postdta_sel)
  ddf$period <- as.numeric(ddf$period)
  ddf$treat <- 1*(ddf$G > 0)
  ddf <- ddf[order(ddf$id, ddf$period),]
  
  return(ddf)
  
}

