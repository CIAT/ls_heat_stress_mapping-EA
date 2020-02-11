# calculate THI future frequency author: John Mutua, CIAT

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

lapply(X=livestockLS, FUN = function(livestock){
  
  lapply(X=rcpLS, FUN = function(rcp){
    
    lapply(X=periodLS, FUN = function(period){
      
      # create output folder current outputs
      oDir <- paste0(iDir, "/output/future/", livestock, "/", rcp, 
                     "/frequency/", period, "/", sep = "")
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
      
      lapply(X=yrLS, FUN = function(yr){
        
        s_yr <- paste0("thi_", yr)  #small fix
        
        # list all thi files
        thi_files <- list.files(paste0(iDir, "/output/future/", 
                                       livestock, "/", rcp, "/thi/", period, "/"), full.names = TRUE, 
                                recursive = TRUE)
        
        # check in file list exists
        if (!file.exists(thi_files[grep(s_yr, thi_files, fixed = TRUE)])) 
          next
        
        # stack thi file
        thi_stack <- stack(thi_files[grep(s_yr, thi_files, fixed = TRUE)])
        
        # cat('Processing frequencies: ', livestock, rcp, period, yr, '\n')
        
        if (livestock == "dairy_cattle") {
          
          # everything per pixel
          normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountDairyCattle))
          p_normal <- normal_count/(nlayers(thi_stack) + 1)  #empirical probability
          writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                                  yr), format = "GTiff", overwrite = TRUE)
          
          mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountDairyCattle))
          p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
          writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                                yr), format = "GTiff", overwrite = TRUE)
          
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
            p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                            1)  #empirical probability
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
              p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                              1)  #empirical probability
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
                p_normal <- normal_count/(nlayers(thi_stack) + 
                                            1)  #empirical probability
                writeRaster(p_normal, filename = paste0(oDir, "p_normal_", 
                                                        yr), format = "GTiff", overwrite = TRUE)
                
                mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountGoat))
                p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
                writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                                      yr), format = "GTiff", overwrite = TRUE)
                
                moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountGoat))
                p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                                1)  #empirical probability
                writeRaster(p_moderate, filename = paste0(oDir, 
                                                          "p_moderate_", yr), format = "GTiff", overwrite = TRUE)
                
                severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountGoat))
                p_severe <- severe_count/(nlayers(thi_stack) + 
                                            1)  #empirical probability
                writeRaster(p_severe, filename = paste0(oDir, "p_severe_", 
                                                        yr), format = "GTiff", overwrite = TRUE)
                
                
              } else {
                
                if (livestock == "poultry") {
                  
                  # everything per pixel
                  normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountPoultry))
                  p_normal <- normal_count/(nlayers(thi_stack) + 
                                              1)  #empirical probability
                  writeRaster(p_normal, filename = paste0(oDir, 
                                                          "p_normal_", yr), format = "GTiff", overwrite = TRUE)
                  
                  mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountPoultry))
                  p_mild <- mild_count/(nlayers(thi_stack) + 1)  #empirical probability
                  writeRaster(p_mild, filename = paste0(oDir, "p_mild_", 
                                                        yr), format = "GTiff", overwrite = TRUE)
                  
                  moderate_count <- clusterR(thi_stack, calc, args = list(fun = ModerateCountPoultry))
                  p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                                  1)  #empirical probability
                  writeRaster(p_moderate, filename = paste0(oDir, 
                                                            "p_moderate_", yr), format = "GTiff", overwrite = TRUE)
                  
                  severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountPoultry))
                  p_severe <- severe_count/(nlayers(thi_stack) + 
                                              1)  #empirical probability
                  writeRaster(p_severe, filename = paste0(oDir, 
                                                          "p_severe_", yr), format = "GTiff", overwrite = TRUE)
                  
                } else {
                  
                  if (livestock == "swine") {
                    
                    # everything per pixel
                    normal_count <- clusterR(thi_stack, calc, args = list(fun = NormalCountSwine))
                    p_normal <- normal_count/(nlayers(thi_stack) + 
                                                1)  #empirical probability
                    writeRaster(p_normal, filename = paste0(oDir, 
                                                            "p_normal_", yr), format = "GTiff", overwrite = TRUE)
                    
                    mild_count <- clusterR(thi_stack, calc, args = list(fun = MildCountSwine))
                    p_mild <- mild_count/(nlayers(thi_stack) + 
                                            1)  #empirical probability
                    writeRaster(p_mild, filename = paste0(oDir, 
                                                          "p_mild_", yr), format = "GTiff", overwrite = TRUE)
                    
                    moderate_count <- clusterR(thi_stack, calc, 
                                               args = list(fun = ModerateCountSwine))
                    p_moderate <- moderate_count/(nlayers(thi_stack) + 
                                                    1)  #empirical probability
                    writeRaster(p_moderate, filename = paste0(oDir, 
                                                              "p_moderate_", yr), format = "GTiff", overwrite = TRUE)
                    
                    severe_count <- clusterR(thi_stack, calc, args = list(fun = SevereCountSwine))
                    p_severe <- severe_count/(nlayers(thi_stack) + 
                                                1)  #empirical probability
                    writeRaster(p_severe, filename = paste0(oDir, 
                                                            "p_severe_", yr), format = "GTiff", overwrite = TRUE)
                    
                  } else {
                    NA
                    
                  }
                }
              }
            }
          }
        }
        
        cat(paste("Processed frequency: ", livestock, rcp, period, 
                  yr, "\n"))
        
      })
      
      # means
      p_normal_stack <- stack(list.files(paste0(oDir), pattern = glob2rx("p_normal*.tif"), 
                                         full.names = TRUE))  #load thi frequency layers
      p_normal_mean <- mean(p_normal_stack, na.rm = TRUE) * 100
      writeRaster(p_normal_mean, paste(oDir, "p_normal_avg", ".tif", 
                                       sep = ""), overwrite = TRUE, format = "GTiff")
      
      p_mild_stack <- stack(list.files(paste0(oDir), pattern = glob2rx("p_mild*.tif"), 
                                       full.names = TRUE))  #load thi frequency layers
      p_mild_mean <- mean(p_mild_stack, na.rm = TRUE) * 100
      writeRaster(p_mild_mean, paste(oDir, "p_mild_avg", ".tif", 
                                     sep = ""), overwrite = TRUE, format = "GTiff")
      
      p_moderate_stack <- stack(list.files(paste0(oDir), pattern = glob2rx("p_moderate*.tif"), 
                                           full.names = TRUE))  #load thi frequency layers
      p_moderate_mean <- mean(p_moderate_stack, na.rm = TRUE) * 100
      writeRaster(p_moderate_mean, paste(oDir, "p_moderate_avg", 
                                         ".tif", sep = ""), overwrite = TRUE, format = "GTiff")
      
      p_severe_stack <- stack(list.files(paste0(oDir), pattern = glob2rx("p_severe*.tif"), 
                                         full.names = TRUE))  #load thi frequency layers
      p_severe_mean <- mean(p_severe_stack, na.rm = TRUE) * 100
      writeRaster(p_severe_mean, paste(oDir, "p_severe_avg", ".tif", 
                                       sep = ""), overwrite = TRUE, format = "GTiff")
      
      cat(paste("Processed means: ", livestock, rcp, period, "\n"))
      
      
    })
    
    
  })
  
  
})