rm(list=ls())

library(tidyverse)
library(magrittr)
library(plm)
library(simhelpers)

alpha <- 1
gamma <- -1
beta1 <- 2
beta2 <- -2
theta <- 1
delta <- 5

p_r2 <- 0.7
p_s <- 0.2
p_a <- 0.2

# Data generation ---------------------------------------------------------

# generate the potential outcome data in one iteration                                                                                                                                                   

generate_dat <- function(n) {
  
  dat <- tibble(
    ID = rep(c(1:n), each=2) %>% rep( 2),
    t = c(rep(0, 2*n), rep(1, 2*n)),
    d = rep(c(0, 1), 2*n),
    C = rnorm(n, mean=0, sd=1) %>% rep( 2) %>% rep( each=2),
    U = rnorm(4*n, mean=0, sd=1),
    E = rnorm(n, mean=0, sd=1) %>% rep( 2) %>% rep( each=2),
    D = 1*I(gamma + beta1 + beta2 + theta + delta*C + E > 0),
    Y = alpha + beta1*t + beta2*D + theta*t*d + D*C + U,
    t_d = t*d
  ) 

  return(dat)
  
}

# Data selection ---------------------------------------------------------

# generate the responded data in one iteration

SampleSelect <- function(dat){
  
  # R2
  R2 <- dat %>%
    filter(d == D) %>%
    group_by(t) %>%
    do(.[sample(nrow(.), p_r2*nrow(.)), ]) %>%
    ungroup() %>%
    do(mutate(.,crit = rep('R2',nrow(.))))
  
  # SSel
  SSel<- do.call(rbind,list(
    dat %>%
      filter(d == D) %>%
      group_by(t) %>%
      filter(C < 0) %>%
      ungroup(),
    dat %>%
      filter(d == D) %>%
      group_by(t) %>%
      filter(C >= 0) %>%
      do(.[sample(nrow(.), p_s*nrow(.)), ]) %>%
      ungroup()
    )
    )%>% do(mutate(.,crit = rep('SSel',nrow(.))))
  
  # AT
  AT<- do.call(rbind,list(
    dat %>%
      filter(d == D, t == 0),
    dat %>%
      filter(d == D, t == 1, C < 0),
    dat %>%
      filter(d == D, t == 1, C >= 0) %>%
      do(.[sample(nrow(.), p_s*nrow(.)), ])
    )
    )%>% do(mutate(.,crit = rep('AT',nrow(.))))
  
  #Asym
  Asym<- do.call(rbind,list(
    dat %>%
      filter(d == D, t == 0, C < 0),
    dat %>%
      filter(d == D, t == 0, C >= 0) %>%
      do(.[sample(nrow(.), p_a*nrow(.)), ]),
    dat %>%
      filter(d == D, t == 1, C >= 0),
    dat %>%
      filter(d == D, t == 1, C < 0) %>%
      do(.[sample(nrow(.), p_a*nrow(.)), ])
    )
  )%>% do(mutate(.,crit = rep('Asym',nrow(.))))
  
  return(do.call(rbind,list(R2, SSel, AT, Asym)))
}


# Estimation Procedures ---------------------------------------------------

# return estimator and var from linear regression and with-in estimation

Reg <- function(dat){
  
  OLS <-  lm(Y ~ t + D + t_d ,
     data = dat) 
  
  FE <- plm(Y ~ t + D + t_d ,
      data = dat,
      model = "within",
      index = c('ID','t'))
  
  Result <- tibble(
    est = c(coef(OLS)['t_d'],
            coef(FE)['t_d']),
    var = c(diag(vcov(OLS))['t_d'],
            diag(vcov(FE))['t_d']),
    Type = c('OLS','FE')
  )
  
  return(Result)
}

# return the balanced data 

Balan <- function(dat){
  
  Bal <- dat %>%
    group_by(ID) %>%
    mutate(name_count = n()) %>%
    ungroup() %>% 
    filter(name_count == 2) %>% 
    select(-name_count)
  
  return(Bal)
}


# Iteration ---------------------------------------------------

# return the results based on each sample selection

Est <- function(dat){
  
  dat_obs <- dat %>% filter(d == D)
  dat_res <- SampleSelect(dat)
  
  # All Data
  result <- Reg(dat_obs) %>%
    mutate(crit = 'All', bal='All')
  
  for (crite in c('R2','SSel','AT','Asym')){
    
    # Balanced Data
    res_bal <- dat_res %>%
      filter(crit == crite) %>%
      Balan() %>%
      Reg() %>%
      mutate(crit = crite, bal='bal')
    
    # UnBalanced Data
    res_unbal <- dat_res %>%
      filter(crit == crite) %>%
      Reg() %>%
      mutate(crit = crite, bal='unbal')
    
    result <- do.call(rbind,list(result, res_bal, res_unbal))
  }
  return(result)
}

# Simulation Driver -------------------------------------------------------

run_sim <- function(iterations, n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  results <-
    map_dfr(1:iterations, ~ {
      dat <- generate_dat(n)
      Est(dat)
    })
  return(results)
}


# Result ---------------------------------------------------

system.time(
  MonteSim <- run_sim(10000,5000) %>%
    mutate(truePar = 1)
)

# All data
MonteSim %>% 
  filter(crit == "All") %>% 
  group_by(Type) %>%
  do(calc_absolute(., estimates = est, true_param = truePar))

# criteria for sample selection and balanced data
MonteSim %>% 
  filter(crit != "All") %>%
  group_by(crit, Type, bal) %>%
  do(calc_absolute(., estimates = est, true_param = truePar)) %>%
  filter(!(Type == "FE" && bal == 'unbal'))

