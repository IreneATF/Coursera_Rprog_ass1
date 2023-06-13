cat("\014") # Clearing console

  #### PART 1 ####

library(stringr)

cat("PART 1\n\n")

pollutantmean <- function(directory,pollutant,id = 1:332) {
  
  setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass1") ## setting directory to location specdata folder  

  print(R.version.string)
  
  setwd(file.path(getwd(),directory)) ## Setting working directory
  
  pollutant_full <- c()
  
  for (x in id) {                                           ## Reading selected group of monitor id 
    
    if (x < 10) {x <- sub('^(.{0})(.?)$', '\\100\\2', x)  ## Adding zeros to match ID file name
    x
    } else if (x < 100) {x <- sub('^(.{0})(.?)$', '\\10\\2', x)
    x
    } else { x <- as.character(x)
    x}
    
    x_c <- c(x,".csv")
    file <- str_flatten(x_c)
    data <- read.csv(file)          ## Read table from one ID
    pollutant_v <- c(data$pollutant)                      ## Creating numerical vector from values of the right pollutant
    bad <- is.na(pollutant_v) 
    pollutant_clean <- c(pollutant_v[!bad])               ## Removing NA values
    pollutant_full<- c(pollutant_full, pollutant_clean)   ## Creating vector with cumulated pollutant values from all IDs
  }
    
    result <- mean(pollutant_full)                        ## Taking mean for all IDs
    result
}
  
source("pollutantmean.R")

## Expected output test

pollutantmean("specdata","sulfate", 1:10)
  #Answer: 4.064128
pollutantmean("specdata", "nitrate", 70:72)
  #Answer: 1.706047
pollutantmean("specdata", "nitrate", 23)
  #Answer: 1.280833

  #### PART 2 ####

cat("PART 2\n\n")

complete <- function(directory, id = 1:332) {
  
  print(R.version.string)
  
  setwd(file.path(getwd(),directory))
}
source("complete.R")
complete("specdata", 1)
  # Answer:   id  nobs
          # 1  1   117

complete("specdata", c(2,4,8,10,12))
## Answer: 
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96

complete("specdata", 30:25)
## Answer:
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463

complete("specdata", 3)
## Answer: 
##   id nobs
## 1  3  243

  #### PART 3 ####

cat("PART 3\n\n")

corr <- function(directory, threshold = 0) {
  
  print(R.version.string)
  
  setwd(file.path(getwd(),directory))
  
}

source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
  ## Answer: 
  ## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814

summary(cr)
  ## Answer:  
  ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  ## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313

cr <- corr("specdata", 400)
head(cr)
  ## Answer:
  ## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860

summary(cr)
  ## Answer:
  ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  ## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313

cr <- corr("specdata", 5000)
summary(cr)
  ## Answer:
  ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  ## 

length(cr)
  ## Answer:
  ## [1] 0

cr <- corr("specdata")
summary(cr)
  ## Answer:
  ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
  ## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000

length(cr)
  ## Answer:
  ## [1] 323
