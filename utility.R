utility <- function(v, lambda, P, Pref) {
# Reference dependent utility function
# Parameters:
# v - value, gamma
# P - current price
# Pref - reference price
# Asymmetric utility

#U <- v - (lambda+1)*P + lambda * Pref
# Asymmetric specification:
# If Price is less than reference price then
# there is no gain in utility
if ((P-Pref)<0) { lambda <- 1 }
U <- v - lambda * (P-Pref) - Pref
  
return(U)
}