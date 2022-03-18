source("definition of SIRS model.R")


# Initial population size in each patch
patchPopSize <- 200

# Number of patches
U <- 2

# Duration of simulations (days)
final_time <- 10*365

# Model parameters
params <- c(
  beta = 0.25,         #Transmission rate
  gamma = 0.1,        #Recovery rate
  rho = 0.005,        #Loss of immunity
  epsilon = 0.0003,    #Birth rate
  delta = 0.0003,      #Death rate
  mu_in = 0.2,      #Rate of migration from peripheral patches to central patch
  mu_out = 0.05,     #Rate of migration to peripheral patches from central patch
  
  
  N = patchPopSize
)

# Initial conditions

# This is the distribution we want (shown here with U=2):
initial_state <- c(
  S1=198, I1=2, R1=0, J1=2,
  S2=200, I2=0, R2=0, J2=0
)
# loop of multiple simulations
nsim <- 1000
total_cases <- matrix(nrow=nsim, ncol=2)
ext_time <- matrix(nrow=nsim, ncol=1)

for (i in 1:nsim)
{
  print(i)
  out <- ssa(
    initial_state = initial_state,
    reactions = c(reactions_intra, reactions_inter),
    params = params,
    final_time = final_time,
    method = ssa_etl(tau=0.5),
    sim_name = "SIRS metapopulation model"
  ) 
  
  
  # Total infections in patch 1 and 2
  total_cases[i,] <- out$state[nrow(out$state), c(4,8)]

  # Time of pathogen extinction
  cases <- (out$state[,2] + out$state[,6])
  if(length(which(cases==0))==0){
    ext_time[i] <- NA
  } else {
    ext_time[i] <- out$time [which(cases==0)[1]]
  }

}

     

