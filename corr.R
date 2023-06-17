cat("\014") ## Clearing console

  #### PART 3 ####

## cat("PART 3\n\n")

corr <- function(directory, threshold = 0) {
  
  library(stringr)
  ## setting directory to location specdata folder  
  setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass1")
  
  print(R.version.string)
  
  setwd(file.path(getwd(),directory)) ## Setting working directory
  
  folder <- getwd()
  
  files <-  list.files(folder, pattern = ".", all.files = FALSE, recursive = TRUE, full.names = TRUE)
  nbr_files <- length(files)
  
  complete_df <- complete(directory,1:nbr_files)
  id_v <- c(complete_df$id)
  nobs_v <- c(complete_df$nobs)
  l <- length(nobs_v)
  
  nitrate_vth <- c()
  sulfate_vth <- c()
  
  for (x in 1:l) {
    if (nobs_v[[x]] > threshold) {
      id <- id_v[[x]]
      
      if (id < 10) {id_c <- paste0("00",id)  ## Adding zeros to match ID file name
      } else if (id < 100) {
        id_c <- paste0("0",id)
      } else {id_c <- id}
      
      id_c <- as.character(id_c)
      id_c <- c(id_c,".csv")
      file <- str_flatten(id_c)
      data <- read.csv(file)
      
      rows <- nrow(data)
      ## pollutant_m <- matrix(c(data$sulfate,data$nitrate), nrow = rows, ncol = 2)
      ## bad <- is.na(pollutant_m)
      
      sulfate_v <- c(data$sulfate)
      nitrate_v <- c(data$nitrate)
      
      for (y in 1:rows) {
        if (is.na(sulfate_v[[y]]) == FALSE & is.na(nitrate_v[[y]]) == FALSE) {
          sulfate_vth <- c(sulfate_vth,sulfate_v[[y]])
          nitrate_vth <- c(nitrate_vth,nitrate_v[[y]])
        }
      }
    }
  
  result <- cor(x = sulfate_vth,y = nitrate_vth)
  result
  }
  
}

setwd("C:/Users/irene/Documents/Lifelong learning/Learning R/R Learning Coursera/Coursera_Rprog_ass1")

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
