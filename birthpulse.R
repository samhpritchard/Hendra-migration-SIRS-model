# Model parameters
params <- c(
  beta = 0.2,         #Transmission rate
  gamma = 0.1,        #Recovery rate
  rho = 0.005,        #Loss of immunity
  delta = 0.0003,      #Death rate
  mu_in = 0.01,      #Rate of migration from peripheral patches to central patch
  mu_out = 0.01,     #Rate of migration to peripheral patches from central patch
  phi = 1.2,         # Timing of birth pulse
  s = 120,            # Distribution parameter of birth pulse
  
  N = patchPopSize
)
# Birth pulse function
birthpulse <- function(t, phi, delta, s){
  
  if(s==0){
    birth <- delta
  } else {
    ki <- delta/besselI(s/2,0,TRUE) 
    cos_term <- exp(-s*(cos(-phi+pi*(t/365)))^2)
    birth <- ki*cos_term
  }
  return(birth)
}
t <- (0:365)
birthrate <- sapply(t, birthpulse, phi = 1.2, delta = 0.0003, s = 120)
plot(t, birthrate, type="l")
