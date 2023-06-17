cat("\014") # Clearing console

  #### PART 2 ####

# cat("PART 2\n\n")

complete <- function(directory, id = 1:332) {
  
  library(stringr)
  ## setting directory to location specdata folder  
  setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass1")
  
  print(R.version.string)
  
  setwd(file.path(getwd(),directory)) ## Setting working directory
  
  id_v <- c()
  nobs_v <- c()
  
  for (x in id) {                                           ## Reading selected group of monitor id 
    id_v <- c(id_v,x)
    
    if (x < 10) {x_c <- paste0("00",x)  ## Adding zeros to match ID file name
    } else if (x < 100) {
      x_c <- paste0("0",x)
    } else {x_c <- x}
    
    x_c <- as.character(x_c)
    x_c <- c(x_c,".csv")
    file <- str_flatten(x_c)
    data <- read.csv(file)          ## Read table from one ID
    
    rows <- nrow(data)
    pollutant_m <- matrix(c(data$sulfate,data$nitrate), nrow = rows, ncol = 2)
    bad <- is.na(pollutant_m)
    nobs <- 0
   
    for (y in 1:rows) {
      if (bad[y,1] == FALSE & bad[y,2] == FALSE) {nobs <- nobs + 1}
    }
    nobs_v <- c(nobs_v,nobs)
  }  
  result <- data.frame(id_v,nobs_v)
  colnames(result) <- c("id","nobs")
  result
}

setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass1")
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
