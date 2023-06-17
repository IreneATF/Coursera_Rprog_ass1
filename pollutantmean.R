cat("\014") ## Clearing console

  #### PART 1 ####

## cat("PART 1\n\n")

pollutantmean <- function(directory,pollutant,id = 1:332) {
  
  library(stringr)
   ## setting directory to location specdata folder  
  setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass1")
  
  print(R.version.string)
  
  setwd(file.path(getwd(),directory)) ## Setting working directory
  
  pollutant_full <- c()
  
  for (x in id) {                                           ## Reading selected group of monitor id 
    
    if (x < 10) {x <- paste0("00",x)  ## Adding zeros to match ID file name
    } else if (x < 100) {
      x <- paste0("0",x)
    } else {x <- x}
    
    x <- as.character(x)
    x_c <- c(x,".csv")
    file <- str_flatten(x_c)
    data <- read.csv(file)          ## Read table from one ID
    
    if (pollutant == "nitrate") {pollutant_v <- c(data$nitrate)         ## Creating numerical vector from values of the right pollutant
    } else if (pollutant == "sulfate") {pollutant_v <- c(data$sulfate)}
                          
    bad <- is.na(pollutant_v) 
    pollutant_clean <- c(pollutant_v[!bad])               ## Removing NA values
    pollutant_full<- c(pollutant_full, pollutant_clean)   ## Creating vector with cumulated pollutant values from all IDs
  }
    result <- mean(pollutant_full)                        ## Taking mean for all IDs
    result
}
  
setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass1")
source("pollutantmean.R")

## Expected output test

pollutantmean("specdata","sulfate", 1:10)
  ## Answer: 4.064128
pollutantmean("specdata", "nitrate", 70:72)
  ## Answer: 1.706047
pollutantmean("specdata", "nitrate", 23)
  ## Answer: 1.280833

