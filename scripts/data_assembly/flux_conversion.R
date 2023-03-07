# The NEE calculations depend on a complicated unit conversion from ppm/s to 
# umol/(s*m^2). This function does that.
fluxConvert <- function(flux, temp){
  # Properties of atmosphere and IRGA chamber.
  A <- 0.75^2 # m^2
  V <- 0.3*0.75^2 # m^3
  P <- 1000 # kPa
  R <- 8.314 # kJ/mol*K
  # Convert using the equation n = (PV/R)*(1/T+273.15)
  print(flux * (P*V/R)*(1/(temp + 273.15)) * 1/A)
}