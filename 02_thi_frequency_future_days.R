# calculate THI future frequency in days
# author: John Mutua, CIAT

# clear all objects and free up memrory
rm(list = ls(all.names = TRUE))

# functions needed
source("./src/requirements.R")

# functions to calculate frequencies
source("./src/calc_frequencies.R")

# set other variables
iDir <- "."
periodLS <- c("2021_2050", "2071_2100")
rcpLS <- c("RCP4.5", "RCP8.5")
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", 
                 "swine")

lapply(X = livestockLS, FUN = function(livestock) {
  
  lapply(X = rcpLS, FUN = function(rcp) {
    
    lapply(X = periodLS, FUN = function(period) {
      
      # create output folder current outputs
      oDir <- paste0(iDir, "/output/future/", livestock, "/", rcp, 
                     "/frequency/", period, "/days/", sep = "")
      if (!file.exists(oDir)) {
        dir.create(oDir, recursive = T)
      }
      
      if (period == periodLS[1]) {
        
        yrLS <- c(2021:2050)
        
      } else if (period == periodLS[2]) {
        
        yrLS <- c(2071:2100)
        
      } else {
        NA
      }
      
      # list all thi files
      thi_files <- list.files(paste0(iDir, "/output/future/", livestock, 
                                     "/", rcp, "/thi/", period, "/"), full.names = TRUE, recursive = TRUE)
      
      beginCluster()
      
      lapply(X = yrLS, FUN = function(yr) {
        
        s_yr <- paste0("thi_", yr)  #small fix
        
        # check in file list exists
        if (!file.exists(thi_files[grep(s_yr, thi_files, fixed = TRUE)])) 
          next
        
        # stack thi file
        thi_stack <- stack(thi_files[grep(s_yr, thi_files, fixed = TRUE)])
        
        # check if output exists
        if (!file.exists(paste(oDir, "severe_days_", yr, ".tif", 
                               sep = ""))) {
          
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
                      severe_count <- clusterR(thi_stack, calc, 
                                               args = list(fun = SevereCountSwine))
                      
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
          cat(paste("Processed frequency: ", livestock, rcp, period, 
                    yr, "\n"))
          
        } else {
          cat(paste("Processed frequency: ", livestock, rcp, period, 
                    yr, "\n"))
        }
        
        
        
      })
      
      endCluster()
      
      severe_stack <- stack(list.files(paste0(oDir), pattern = glob2rx("severe_days*.tif"), 
                                       full.names = TRUE))  #load thi frequency layers
      severe_mean <- mean(severe_stack, na.rm = TRUE)
      writeRaster(severe_mean, paste(oDir, "severe_avg_days", ".tif", 
                                     sep = ""), overwrite = TRUE, format = "GTiff")
      
      cat(paste("Processed means: ", livestock, rcp, period, "\n"))
      
      
    })
    
    
  })
  
  
})
