
#
# R-marginal density function for prior (see mandatory assignment 2)
#
dltrunc_normal <- function(r,l,mu,sigma){
  Fl <- pnorm(q=l,mean=mu,sd=sigma)
  return(dnorm(x=r,mean=mu,sd=sigma)*(r>=l)/(1.0-Fl))
}
#
# joint probability density function for the prior (see mandatory assignment 2)
#
dRS <- function(r,s){
  if(r<0.0) return(0.0)
  return(dltrunc_normal(r,1.8,2.2,1.5)*dunif(s,min=0.5/r,max=0.8/r))
}

# load seir model functions
source("seir_model.R")

# load hospital-data
df <- data.frame(read.table("SEIR_hospital_data.txt"))
dayNum <- df$dayNum
numHosp <- df$numHosp

#
# define a function evaluating the (R,S)-log-posterior density kernel
#

seir.lp <- function(theta){
  # extract indiviual parameters from theta
  R <- theta[1]
  S <- theta[2]
  
  # evaluate prior density
  prior.dens <- as.numeric(dRS(r=R,s=S))
  
  # if the prior density is zero, return effectively -infinity
  # without running the seir model
  if(prior.dens<1.0e-14) return(-1e100)
  
  # run the SEIR model with the proposed parameters
  seir.out <- seir_sim(R0max=R,soc_dist_Rfac = S)
  
  # extract hospital states
  modelHosp <- 1000000*(seir.out$ode$HH[dayNum]+
                          seir.out$ode$HC[dayNum]+
                          seir.out$ode$CC[dayNum])
  
  # log-likelihood
  log.like <- sum(dpois(numHosp,lambda = modelHosp,log=TRUE))
  
  # return posterior kernel = log-likelihood + log(p(theta))
  return(log.like+log(prior.dens))
}
