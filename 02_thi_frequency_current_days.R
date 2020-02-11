# calculate historical severe THI frequency of sever and danger days
# author: John Mutua, CIAT

# some clean up
options(warn = -1); options(scipen = 999); rm(list = ls())

# functions needed
source("./src/requirements.R")

# functions to calculate frequencies
source("./src/calc_frequencies.R")

# set other variables
iDir <- "."
yrLS <- c(1981:2010)
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", "swine") 

#create a date dataset
dateLS <- seq(as.Date("1981-1-1"), as.Date("2010-12-31"), by = "days")
dateLS <- data.frame(dateLS) 
dateLS$year <- substr(dateLS$dateLS, 1, 4); dateLS$month <- substr(dateLS$dateLS, 6, 7); dateLS$day <- substr(dateLS$dateLS, 9, 10)
dateLS$date <- gsub("-", ".", dateLS$dateLS)

for (livestock in livestockLS){
  
  # create output folder current outputs
  oDir <- paste0(iDir, "/output/historical/", livestock, "/frequency/", "days/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  for (yr in yrLS){
    
    beginCluster()
    
    #subset dateLS by year and create a vector and substract month and day
    yr_dates <- subset(dateLS, year == yr); date_vec <- substr(c(yr_dates$date), 5, 10)
    
    #load thi layers
    thi_stack <- stack(paste0(iDir, "/output/historical/", livestock, "/thi/", "thi_", yr, date_vec, ".tif"))
    
    cat("Processing frequencies: ", livestock, yr, "\n")
    
    if (livestock == "dairy_cattle") {
      
      # everything per pixel
      severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountDairyCattle))

    } else {
      
      if (livestock == "beef_cattle") {
        
        # everything per pixel
        severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountBeefCattle))

      } else {
        
        if (livestock == "poultry") {
          
          # everything per pixel
          severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountPoultry))

        } else {
          
          if (livestock == "sheep") {
            
            # everything per pixel
            severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountSheep))

          } else {
            
            if (livestock == "goat") {
              
              # everything per pixel
              severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountGoat))

            } else {
              
              if (livestock == "swine") {
                
                # everything per pixel
                severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountSwine))

              } else {
                NA
              }
            }
          }
        }
      }
    }
    
    writeRaster(severe_count, filename = paste0(oDir, "severe_days_", 
                                                yr), format = "GTiff", overwrite = TRUE)
    
    endCluster()
  }
  
  #means
  severe_stack <- stack(paste0(oDir, "severe_days_", 1981:2010, ".tif")) #load thi frequency layers
  severe_mean <- mean(severe_stack, na.rm=TRUE)
  writeRaster(severe_mean, paste(oDir, "severe_avg_days", ".tif", sep=""), overwrite=TRUE, format="GTiff")
  
  cat(paste("Processed means: ", livestock, "\n"))
  
}