# Function to create a time series object
# val_array: data array for one single pixel (length is number of time steps)
# time_array: array with dates at which raster data is recorded (same length as val_array)

timeser <- function(val_array, time_array){
  
  ## create zoo object 
  z <- zoo(val_array, time_array)
  
  ## extract the year numbers
  yr <- as.numeric(format(time(z), "%Y"))
  
  ## extract the day numbers (1-365)
  jul <- as.numeric(format(time(z), "%j"))
  
  ## calculate minimum time difference (days) between observations
  delta <- min(unlist(tapply(jul, yr, diff)))
  
  ## aggregate into decimal year timestamps
  zz <- aggregate(z, yr + (jul - 1) / delta / 23)
  
  ## convert into time series object
  (tso <- as.ts(zz))
  return(tso)
}