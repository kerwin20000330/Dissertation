rm(list=ls())
library(did)
library(tidyverse)
library(magrittr)
library(simhelpers)

# 1 Generate data
generate_dat(100,4,4)

# 2 Run regression
set.seed(1234)
dat <- generate_dat(5000,4,4)
Reg(dat)
Est(dat)
CleanProcess(dat)

example_attgt <- Reg(dat)
ggdid(example_attgt)

agg.simple <- aggte(example_attgt, type = "simple")
summary(agg.simple)

agg.es <- aggte(example_attgt, type = "dynamic")
summary(agg.es)
ggdid(agg.es)

agg.gs <- aggte(example_attgt, type = "group")
summary(agg.gs)
ggdid(agg.gs)

agg.ct <- aggte(example_attgt, type = "calendar")
summary(agg.ct)
ggdid(agg.ct)

#3 Simulation
system.time(
  MonteSim <- run_sim(200,1000,4,4,seed = 1234) %>%
    mutate(truePar = 1)
)

MonteSim %>% 
  group_by(Type) %>%
  do(calc_absolute(., estimates = est, true_param = truePar))


system.time(
  MonteSim_sel <- run_sim_sel(200,5000,4,4,
                              seed = 1234,
                              pr = list(prPreU = 0.2,
                                        prPreL = 1,
                                        prPostU = 1,
                                        prPostL = 0.2)) %>%
    mutate(truePar = 1)
)

MonteSim_sel %>%
  filter(data == 'Unbalanced') %>%
  group_by(Type) %>%
  do(calc_absolute(., estimates = est, true_param = truePar))

MonteSim_sel %>%
  filter(data == 'Balanced') %>%
  group_by(Type) %>%
  do(calc_absolute(., estimates = est, true_param = truePar))
