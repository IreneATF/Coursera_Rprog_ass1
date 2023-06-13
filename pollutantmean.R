cat("\014") # Clearing console

pollutantmean <- function(directory,pollutant,id = 1:332) {
  
  print(R.version.string)
  
  setwd(...directory)
  
  for x in id {
    
    data <- read.csv(id)
    bad <- is.na(x)
    x[!bad]
    mean(pollutant)
  }
  
}

source("pollutantmean.R")

## Expected output test

pollutantmean("specdata","sulfate", 1:10)
  #Answer: 4.064128
pollutantmean("specdata", "nitrate", 70:72)
  #Answer: 1.706047
pollutantmean("specdata", "nitrate", 23)
  #Answer: 1.280833
