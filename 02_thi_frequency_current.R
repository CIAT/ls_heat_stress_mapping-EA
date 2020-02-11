# calculate THI frequency author: John Mutua, CIAT

# clear your work space
rm(list = ls(all = TRUE))

# functions needed
source("./src/requirements.R")

# functions to calculate frequencies
source("./src/calc_frequencies.R")

# set other variables
iDir <- "."
yrLS <- c(1981:2010)
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", 
                 "goat", "swine")

# create a date dataset
dateLS <- seq(as.Date("1981-1-1"), as.Date("2010-12-31"), by = "days")
dateLS <- data.frame(dateLS)
dateLS$year <- substr(dateLS$dateLS, 1, 4)
dateLS$month <- substr(dateLS$dateLS, 6, 7)
dateLS$day <- substr(dateLS$dateLS, 9, 10)
dateLS$date <- gsub("-", ".", dateLS$dateLS)

for (livestock in livestockLS) {
  
  beginCluster()
  
  # create output folder current outputs
  oDir <- paste0(iDir, "/output/historical/", livestock, "/frequency/", 
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
    
    cat("Processing frequencies: ", livestock, yr, "\n")
    
    if (livestock == "dairy_cattle") {
      
      # everything per pixel
      normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountDairyCattle))
      p_normal <- normal_count/(nlayers(thi_stack) + 1)  #empirical probability
      writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                              yr), format = "GTiff", overwrite = TRUE)
      
      mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountDairyCattle))
      p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
      writeRaster(p_mild, filename = paste0(oDir, "p_mild_", yr), 
                  format = "GTiff", overwrite = TRUE)
      
      moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountDairyCattle))
      p_moderate <- moderate_count/(nlayers(thi_stack) + 1)  #empirical probability
      writeRaster(p_moderate, filename = paste0(oDir, "p_moderate_", 
                                                yr), format = "GTiff", overwrite = TRUE)
      
      severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountDairyCattle))
      p_severe <- severe_count/(nlayers(thi_stack) + 1)  #empirical probability
      writeRaster(p_severe, filename = paste0(oDir, "p_severe_", 
                                              yr), format = "GTiff", overwrite = TRUE)
      
      
    } else {
      
      if (livestock == "beef_cattle") {
        
        # everything per pixel
        normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountBeefCattle))
        p_normal <- normal_count/(nlayers(thi_stack) + 1)  #empirical probability
        writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                                yr), format = "GTiff", overwrite = TRUE)
        
        mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountBeefCattle))
        p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
        writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                              yr), format = "GTiff", overwrite = TRUE)
        
        moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountBeefCattle))
        p_moderate <- moderate_count/(nlayers(thi_stack) + 1)  #empirical probability
        writeRaster(p_moderate, filename = paste0(oDir, "p_moderate_", 
                                                  yr), format = "GTiff", overwrite = TRUE)
        
        severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountBeefCattle))
        p_severe <- severe_count/(nlayers(thi_stack) + 1)  #empirical probability
        writeRaster(p_severe, filename = paste0(oDir, "p_severe_", 
                                                yr), format = "GTiff", overwrite = TRUE)
        
      } else {
        
        if (livestock == "sheep") {
          
          # everything per pixel
          normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountSheep))
          p_normal <- normal_count/(nlayers(thi_stack) + 1)  #empirical probability
          writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                                  yr), format = "GTiff", overwrite = TRUE)
          
          mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountSheep))
          p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
          writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                                yr), format = "GTiff", overwrite = TRUE)
          
          moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountSheep))
          p_moderate <- moderate_count/(nlayers(thi_stack) + 1)  #empirical probability
          writeRaster(p_moderate, filename = paste0(oDir, "p_moderate_", 
                                                    yr), format = "GTiff", overwrite = TRUE)
          
          severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountSheep))
          p_severe <- severe_count/(nlayers(thi_stack) + 1)  #empirical probability
          writeRaster(p_severe, filename = paste0(oDir, "p_severe_", 
                                                  yr), format = "GTiff", overwrite = TRUE)
          
        } else {
          
          if (livestock == "goat") {
            
            # everything per pixel
            normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountGoat))
            p_normal <- normal_count/(nlayers(thi_stack) + 1)  #empirical probability
            writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                                    yr), format = "GTiff", overwrite = TRUE)
            
            mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountGoat))
            p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
            writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                                  yr), format = "GTiff", overwrite = TRUE)
            
            moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountGoat))
            p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                            1)  #empirical probability
            writeRaster(p_moderate, filename = paste0(oDir, "p_moderate_", 
                                                      yr), format = "GTiff", overwrite = TRUE)
            
            severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountGoat))
            p_severe <- severe_count/(nlayers(thi_stack) + 1)  #empirical probability
            writeRaster(p_severe, filename = paste0(oDir, "p_severe_", 
                                                    yr), format = "GTiff", overwrite = TRUE)
            
            
          } else {
            
            if (livestock == "poultry") {
              
              # everything per pixel
              normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountPoultry))
              p_normal <- normal_count/(nlayers(thi_stack) + 1)  #empirical probability
              writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                                      yr), format = "GTiff", overwrite = TRUE)
              
              mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountPoultry))
              p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
              writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                                    yr), format = "GTiff", overwrite = TRUE)
              
              moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountPoultry))
              p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                              1)  #empirical probability
              writeRaster(p_moderate, filename = paste0(oDir, "p_moderate_", 
                                                        yr), format = "GTiff", overwrite = TRUE)
              
              severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountPoultry))
              p_severe <- severe_count/(nlayers(thi_stack) + 1)  #empirical probability
              writeRaster(p_severe, filename = paste0(oDir, "p_severe_", 
                                                      yr), format = "GTiff", overwrite = TRUE)
              
            } else {
              
              if (livestock == "swine") {
                
                # everything per pixel
                normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountSwine))
                p_normal <- normal_count/(nlayers(thi_stack) + 
                                            1)  #empirical probability
                writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                                        yr), format = "GTiff", overwrite = TRUE)
                
                mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountSwine))
                p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
                writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                                      yr), format = "GTiff", overwrite = TRUE)
                
                moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountSwine))
                p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                                1)  #empirical probability
                writeRaster(p_moderate, filename = paste0(oDir, 
                                                          "p_moderate_", yr), format = "GTiff", overwrite = TRUE)
                
                severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountSwine))
                p_severe <- severe_count/(nlayers(thi_stack) + 
                                            1)  #empirical probability
                writeRaster(p_severe, filename = paste0(oDir, "p_severe_", 
                                                        yr), format = "GTiff", overwrite = TRUE)
                
              } else {
                NA
                
              }
            }
          }
        }
      }
    }
  }
  
  # means
  p_normal_stack <- stack(paste0(oDir, "p_normal_", 1981:2010, ".tif"))  #load thi frequency layers
  p_normal_mean <- mean(p_normal_stack, na.rm = TRUE) * 100
  writeRaster(p_normal_mean, paste(oDir, "p_normal_avg", ".tif", sep = ""), 
              overwrite = TRUE, format = "GTiff")
  
  p_mild_stack <- stack(paste0(oDir, "p_mild_", 1981:2010, ".tif"))  #load thi frequency layers
  p_mild_mean <- mean(p_mild_stack, na.rm = TRUE) * 100
  writeRaster(p_mild_mean, paste(oDir, "p_mild_avg", ".tif", sep = ""), 
              overwrite = TRUE, format = "GTiff")
  
  p_moderate_stack <- stack(paste0(oDir, "p_moderate_", 1981:2010, ".tif"))  #load thi frequency layers
  p_moderate_mean <- mean(p_moderate_stack, na.rm = TRUE) * 100
  writeRaster(p_moderate_mean, paste(oDir, "p_moderate_avg", ".tif", 
                                     sep = ""), overwrite = TRUE, format = "GTiff")
  
  p_severe_stack <- stack(paste0(oDir, "p_severe_", 1981:2010, ".tif"))  #load thi frequency layers
  p_severe_mean <- mean(p_severe_stack, na.rm = TRUE) * 100
  writeRaster(p_severe_mean, paste(oDir, "p_severe_avg", ".tif", sep = ""), 
              overwrite = TRUE, format = "GTiff")
  
  cat(paste("Processed means: ", livestock, "\n"))
  
  endCluster()
  
}