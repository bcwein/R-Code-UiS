# Load the required differential equation solver package.
# If you have not already done so, this library must be installed
# using the command 
# > install.packages("deSolve")
#
library (deSolve)


#-----------------------------------------------------------
# Function: seir_sim
#-----------------------------------------------------------
# The main function for simulating the SEIR model dynamics
# See attached pdf-file for more information on the workings
# of this function.
#

seir_sim <- function(R0max = 2.2,
                     soc_dist_Rfac = 0.4 ,
                     season_amplitude = 0.4,
                     upper_thresh = 35,
                     lower_thresh = 5) {
  
  # constants
  dates <- seq(as.Date("2020/1/1"), as.Date("2025/01/01"), by = "day")
  times <- seq(from = 1, to = length(dates))
  mar13 <- which(dates == "2020-03-13")
  
  
  seir_upper_thresh <- upper_thresh
  seir_lower_thresh <- lower_thresh
  
  seir_gamma <- 1.0 / 5.0
  seir_pr <- 0.956
  seir_ph <- 0.0308
  seir_pc <- 0.0132
  seir_nu <- 1.0 / 4.6
  seir_dh <- 1.0 / 8.0
  seir_dc <- 1.0 / 6.0
  seir_xic <- 1.0 / 10.0
  
  seir_R0max <- R0max
  seir_phi <- 2.0
  #seir_f <- season_amplitude
  
  seir_N <- 10000
  
  seir_R0 <- function(t) {
    tt <- t / 7.0 # Kissler et al uses weekly time scale
    return(seir_R0max * (0.5 * season_amplitude * cos(2 * pi / 52 * (tt - seir_phi)) +
                           (1.0 - 0.5 * season_amplitude)))
  }
  seir_beta <- function(t) {
    return(seir_R0(t) * seir_gamma)
  }
  
  seir_soc_dist <- FALSE
  
  seir_ode <- function(t, states, parms) {
    S <- states[1]
    E <- states[2]
    IR <- states[3]
    IH <- states[4]
    IC <- states[5]
    HH <- states[6]
    HC <- states[7]
    CC <- states[8]
    RR <- states[9]
    RH <- states[10]
    RC <- states[11]
    soc_dist <- states[12]
    
    betat <- seir_beta(t)
    if (soc_dist > 0.5)
      betat <- soc_dist_Rfac * betat
    
    intro <- 0.0
    if (t >= mar13 && t < mar13 + 3.5)
      intro <- 14.2857 #0.01/7*10000
    
    dS <- -betat * (IR + IH + IC) * S / seir_N - intro
    dE <- betat * (IR + IH + IC) * S / seir_N + intro - seir_nu * E
    dIR <- seir_pr * seir_nu * E - seir_gamma * IR
    dIH <- seir_ph * seir_nu * E - seir_gamma * IH
    dIC <- seir_pc * seir_nu * E - seir_gamma * IC
    dHH <- seir_gamma * IH - seir_dh * HH
    dHC <- seir_gamma * IC - seir_dc * HC
    dCC <- seir_dc * HC - seir_xic * CC
    dRR <- seir_gamma * IR
    dRH <- seir_dh * HH
    dRC <- seir_xic * CC
    dsoc_dist <- 0.0
    
    return(list(c(
      dS, dE, dIR, dIH, dIC, dHH, dHC, dCC, dRR, dRH, dRC, dsoc_dist
    )))
    
  }
  
  seir_root <- function(t, states, parms) {
    IR <- states[3]
    IH <- states[4]
    IC <- states[5]
    soc_dist <- states[12]
    ret <-
      soc_dist * (IR + IH + IC - seir_lower_thresh) + (1.0 - soc_dist) * (IR + IH + IC - seir_upper_thresh)
    return(ret)
  }
  
  seir_event <- function(t, states, parms) {
    states_new <- states
    states_new[12] <-
      (states_new[12] + 1) %% 2 # switches either on->off or off->on
    return(states_new)
  }
  
  
  seir_initialCond <- c(seir_N, rep(0.0, 11))
  
  seir_f <- 0.4
  
  out <- lsoda(
    y = seir_initialCond,
    times = times,
    func = seir_ode,
    parms = NULL,
    events = list(func = seir_event, root = TRUE),
    rootfunc = seir_root
  )
  
  out <- as.data.frame(out)
  colnames(out) <- c("time",
                     "S",
                     "E",
                     "IR",
                     "IH",
                     "IC",
                     "HH",
                     "HC",
                     "CC",
                     "RR",
                     "RH",
                     "RC",
                     "soc_dist")
  
  out[,2:12] <- (1.0/seir_N)*out[,2:12]
  return(list(ode=out,dates=dates))
}


#-----------------------------------------------------------
# Function: seir_getStateAtDate
#-----------------------------------------------------------
# Returns the state of the system at any given date.
# The date is expected to be a string on the format
# "YYYY-MM-DD"
# between Jan 1st 2020 and Jan 1st 2025
# e.g. "2021-01-13" is Jan 13th 2021
# Example of usage:
# > f <- seir_sim()
# > seir_getStateAtDate(f,"2021-01-13")


seir_getStateAtDate<- function(obj,date){
  return(obj$ode[which(obj$dates==date),])
}
#-----------------------------------------------------------
# Function: seir_getIndividualState
#-----------------------------------------------------------
# Returns the complete trajetory of any given state.
# The possible states are 
# c("time","S","E","IR","IH","IC","HH","HC","CC","RR",
# "RH","RC","soc_dist")
# Example of usage
# # Example of usage: get trajetory of "susceptible" "S"
# > f <- seir_sim()
# > ts.plot(seir_getIndividualState(f,"S"))
seir_getIndividualState <- function(obj,state){
  return(obj$ode[,state])
}







