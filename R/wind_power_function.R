# function for calculating wind power availability
# author: Jackson Mills

# inputs:
# rho: air density (kg/m^3)
# r: blade length (m)
# v: wind speed (m/s)

# output:
# p: power (MW)

wind_power <- function(rho, r, v){
  
  # error checking:
  # wind speed must be greater than zero
  ifelse(v < 0, stop("Wind speed cannot be negative"), v)
  
  # air density must be greater than zero
  ifelse(rho < 0, stop("Air density cannot be negative"), rho)
  
  # if air density is greater than 2, issue a warning
  ifelse(rho > 2, warning("Air density is unrealistically high"), rho)
  
  # area must be greater than zero
  ifelse(r < 0, stop("Blade length cannot be negative"), r)
  
  # define the power equation using the formula p = 0.5 * rho * A * v^3
  # note: A = pi * r^2
  p = 0.5 * rho * pi * r^2 * v^3
  
  # return the result, dividing by 10^6 to convert to megawatts
  return(p/10^6)
}

