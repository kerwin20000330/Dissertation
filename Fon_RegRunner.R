library(did)
library(tidyverse)
library(magrittr)

# Regression model ---------------------------------------------------

# return results by a did method

Reg <- function(dat, method='DR', xf = ~X, Bal = FALSE){
  
  example_attgt <- att_gt(yname = "Y",
                          tname = "period",
                          idname = "id",
                          gname = "G",
                          xformla = xf,
                          data = dat,
                          est_method = method,
                          control_group="notyettreated",
                          allow_unbalanced_panel = Bal)
  return(example_attgt)
}


# Results cleanance ---------------------------------------------------

# return the estimator we focus, default is ATT(2,2)

Est <- function(dat, method='DR', i=1, xf = ~X, Bal = FALSE){
  example_attgt <- Reg(dat, method, xf, Bal)
  return(c(example_attgt$att[i],example_attgt$se[i]))
}

CleanProcess <- function(dat, i=1, xf = ~X, Bal = FALSE){
  
  res <- mapply(Est, c('DR', 'reg', 'IPW', 'UNC'),
                MoreArgs = list(i = i, dat = dat, xf = xf, Bal = Bal))
  
  Results <- tibble(
    est = c(res[i,1],
            res[i,2],
            res[i,3],
            res[i,4]),
    var = c(res[i,1],
            res[i,2],
            res[i,3],
            res[i,4]),
    Type = c('DR', 'REG', 'IPW', 'UNC'))
  
  return(Results)
}
