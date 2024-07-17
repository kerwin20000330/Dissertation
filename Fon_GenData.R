rm(list=ls())

# Data generation ---------------------------------------------------------

# generate the observed outcome data in one iteration 

generate_dat <- function(n,G,time.periods) {
  
  gamG <- c(0,1:time.periods)/(2*time.periods)
  
  # allow transformation of X, here is just linear X
  X <- rnorm(n, mean = 0, sd = 1)
  Xmodel <- X
  
  # create propensity matrix
  pr <- exp(outer(X,gamG)) / apply(exp(outer(X,gamG)), 1, sum)
  # assign treated group
  G <- apply(pr, 1, 
             function(pvec) sample(seq(0,time.periods), size=1, prob=pvec))
  
  
  # First, focus on treated group
  
  # assign some variable
  Gt <- G[G>0]
  nt <- length(Gt)
  Xt <- Xmodel[G>0]
  
  eta <- rnorm(nt, mean=G, sd =1)
  thetat <- 1:4
  beta <- 1:4
  
  # potential untreated outcome
  Y0tmat <- sapply(1:time.periods, function(t) {
    thetat[t] + eta + Xt*beta[t] + rnorm(nt)
  })
  
  # potential treated outcome
  Y1tmat <- sapply(1:time.periods, function(t) {
    thetat[t] + eta + Xt*beta[t] + (t-Gt+1) + rnorm(nt)
  })
  
  # generate observed data
  Ymat <- sapply(1:time.periods, function(t) {
    (Gt<=t)*Y1tmat[,t] + (Gt>t)*Y0tmat[,t]
  })
  Ytdf <- as.data.frame(Ymat)
  Ynames <- paste0("Y",1:time.periods)
  colnames(Ytdf) <- Ynames
  
  # store observed data for treated group
  dft <- cbind.data.frame(G=Gt,X=X[G>0],Ytdf)
  
  # Second,  focus on untreated group
  
  # assign some variable
  nu <- sum(G==0)
  Xu <- Xmodel[G==0]
  
  eta <- rnorm(nu, mean=G, sd =1)
  
  # untreated potential outcomes
  Y0umat <- sapply(1:time.periods, function(t) {
    thetat[t] + eta + Xu*beta[t] + rnorm(nu)
  })
  Y0udf <- as.data.frame(Y0umat)
  colnames(Y0udf) <- Ynames
  
  # store observed data for untreated group
  dfu <- cbind.data.frame(G=0,X=X[G==0],Y0udf)
  
  # Finally, store overall data set
  df <- rbind.data.frame(dft, dfu)
  
  # generate id variable
  df$id <- 1:nrow(df)
  
  ddf <- tidyr::pivot_longer(df,
                             cols=tidyr::starts_with("Y"),
                             names_to="period",
                             names_prefix="Y",
                             values_to="Y")
  
  ddf$period <- as.numeric(ddf$period)
  ddf$treat <- 1*(ddf$G > 0)
  ddf <- ddf[order(ddf$id, ddf$period),] # reorder data
  
  ddf <- subset(ddf, G != 1) 
  # remove units that were already treated in the first period
  
  return(ddf)
}

