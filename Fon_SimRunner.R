library(did)
library(tidyverse)
library(magrittr)
library(simhelpers)

# Simulation Driver -------------------------------------------------------

run_sim <- function(iterations, n, G, time.period, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  results <-
    map_dfr(1:iterations, ~ {
      dat <- generate_dat(n,G,time.period)
      CleanProcess(dat)
    })
  return(results)
}


run_sim_sel <- function(iterations, n, G, time.period,
                        xf = ~X,
                        seed = NULL,
                        pr = list(prPreU = 0.7,
                                  prPreL = 0.7,
                                  prPostU = 0.7,
                                  prPostL = 0.7)) {
  if (!is.null(seed)) set.seed(seed)
  
  prPreU <- pr$prPreU
  prPreL <- pr$prPreL
  prPostU <- pr$prPostU
  prPostL <- pr$prPostL
  
  results <-
    map_dfr(1:iterations, ~ {
      dat <- generate_dat(n,G,time.period)
      datSel <- Select(dat,0.7,0.7,0.7,0.7)
      CleanProcess(datSel,Bal = TRUE) %>% 
        mutate(data = 'Unbalanced') %>% 
        add_row(CleanProcess(datSel,Bal = FALSE)%>% 
                  mutate(data = 'Balanced'))
    })
  
  
  return(results)
}

