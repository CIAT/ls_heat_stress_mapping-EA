# calculate THI frequency author: John Mutua, CIAT

# clear your work space
rm(list = ls())
options(scipen = 999, warn = -1)
memory.limit(size = 64000)

# libraries
source("./src/requirements.R")

# functions to calculate trend et al
source("./src/calc_trend.R")

# set variables
iDir <- "."
yrLS <- c(1981:2010)
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", 
                 "swine")

# create a date dataset
dateLS <- seq(as.Date("1981-1-1"), as.Date("2010-12-31"), by = "days")
dateLS <- data.frame(dateLS)
dateLS$year <- substr(dateLS$dateLS, 1, 4)
dateLS$month <- substr(dateLS$dateLS, 6, 7)
dateLS$day <- substr(dateLS$dateLS, 9, 10)
dateLS$date <- gsub("-", ".", dateLS$dateLS)

for (livestock in livestockLS) {
  
  # create output folder current outputs
  oDir <- paste0(iDir, "/output/historical/", livestock, "/consecutive_days_main/", 
                 sep = "")
  if (!file.exists(oDir)) {
    dir.create(oDir, recursive = T)
  }
  
  for (yr in yrLS) {
    
    # subset dateLS by year and create a vector and substract month and day
    yr_dates <- subset(dateLS, year == yr)
    date_vec <- substr(c(yr_dates$date), 5, 10)
    
    # load thi layers
    thi_stack <- stack(paste0(iDir, "/output/historical/", livestock, 
                              "/thi/", "thi_", yr, date_vec, ".tif"))
    
    if (livestock == "dairy_cattle") {
      #t <- "85.0"
      t <- "88.0"
    } else {
      if (livestock == "beef_cattle") {
        #t <- "87.0"
        t <- "91.0"
      } else {
        if (livestock == "poultry") {
          t <- "29.9"
        } else {
          if (livestock == "sheep") {
            t <- "35.0"
          } else {
            if (livestock == "goat") {
              #t <- "36.0"
              t <- "37.0"
            } else {
              if (livestock == "swine") {
                t <- "28.0"
              } else {
                NA
              }
            }
          }
        }
      }
    }
    
    if (!file.exists(paste(oDir, "max_cons_severe_days_", yr, ".tif", 
                           sep = ""))) {
      
      cat(paste("Processing consecutive days: ", livestock, yr, "\n"))
      
      # everything per pixel
      max_cons_severe_days <- calc(thi_stack, function(x) max.cons.severe.days(x,t))
      
      # write output
      writeRaster(max_cons_severe_days, filename = paste0(oDir, "max_cons_severe_days_", 
                                                          yr), format = "GTiff", overwrite = TRUE)
      
      
      rm(max_cons_severe_days)
      
    } else {
      cat(paste("Processed consecutive days: ", livestock, yr, "\n"))
    }
    
  }
  
  
  if (!file.exists(paste(oDir, "max_cons_severe_avg_days", ".tif", sep = ""))) {
    
    # means
    cons_stack <- stack(paste0(oDir, "max_cons_severe_days_", 1981:2010, 
                               ".tif"))
    cons_mean <- mean(cons_stack, na.rm = TRUE)
    writeRaster(cons_mean, paste(oDir, "max_cons_severe_avg_days", 
                                 ".tif", sep = ""), overwrite = TRUE, datatype = "INT4S", format = "GTiff")
    
    rm(cons_mean)
    
  } else {
    cat(paste("Processed mean consecutive days: ", livestock, "\n"))
  }
  
}
