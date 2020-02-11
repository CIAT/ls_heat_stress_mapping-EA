# calculate future THI for different livestock species
# author: John Mutua, CIAT
# last modified: 04072019

# clear your work space
rm(list = ls(all = TRUE))

# functions needed
source("./src/requirements.R")

# functions to calculate indices
source("./src/calc_indices.R")

# functions to calculate thresholds
source("./src/calc_thresholds.R")

# set other variables
iDir <- "."
varLS <- c("HURS", "TASMAX")
rcpLS <- c("RCP4.5", "RCP8.5")
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", "swine")

# create a time vector
dateLS <- seq(as.Date("2051-1-1"), as.Date("2070-12-31"), by = "days")
dateLS <- data.frame(dateLS) 
dateLS$year <- substr(dateLS$dateLS, 1, 4); dateLS$month <- substr(dateLS$dateLS, 6, 7); dateLS$day <- substr(dateLS$dateLS, 9, 10)
dateLS$date <- gsub("-", ".", dateLS$dateLS)
dateLS <- c(dateLS$date)

for (livestock in livestockLS){
  
  for (rcp in rcpLS){
    
    # create output folder current outputs
    oDir <- paste0(iDir, "/output/future/", livestock, "/", rcp, "/thi/", sep="")
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
    for (dy in dateLS){
      
      if (!file.exists(paste(oDir, "/thi_", dy, ".tif", sep=""))){
        
        cat("Processing THI: ", livestock, rcp, dy, "\n")
        
        if (!file.exists(paste0(iDir, "/data/future/_tifs/ensembles/", rcp, "/", varLS[1], "/EA__X", dy, ".tif"))) next 
        
        # load relative humidity layer
        rh <- raster(paste0(iDir, "/data/future/_tifs/ensembles/", rcp, "/", varLS[1], "/EA__X", dy, ".tif"))
        y<-rh
        
        if (!file.exists(paste0(iDir, "/data/future/_tifs/ensembles/", rcp, "/", varLS[2], "/EA__X", dy, ".tif"))) next
        
        # read maximum temperature files
        tmax <- raster(paste0(iDir, "/data/future/_tifs/ensembles/", rcp, "/", varLS[2], "/EA__X", dy, ".tif"))
        tmax <- resample(crop(tmax, rh), rh)
        x<-tmax
        
        if (livestock == "dairy_cattle") {
          
          thi <- overlay(x, y, fun = CattleThi)
          # thi_threshold <- calc(thi, fun=PoultryThreshold)
          
        } else {
          
          if (livestock == "beef_cattle") {
            
            thi <- overlay(x, y, fun = CattleThi)
            
          } else {
            
            if (livestock == "poultry") {
              
              thi <- overlay(x, y, fun = PoultryThi)
              
            } else {
              
              if (livestock == "sheep") {
                
                thi <- overlay(x, y, fun = SmallRuminantThi)
                
              } else {
                
                if (livestock == "goat") {
                  
                  thi <- overlay(x, y, fun = SmallRuminantThi)
                  
                } else {
                  if (livestock == "swine") {
                    
                    thi <- overlay(x, y, fun = SwineThi)
                    
                  } else {
                    NA
                  }
                }
              }
            }
          }
        }
        
        # write output
        writeRaster(thi, filename = paste0(oDir, "/thi_", dy, ".tif"), format = "GTiff", overwrite=TRUE)
        #writeRaster(thi_threshold, filename = paste0(oDir, "/thi_threshold_", dy, ".tif"), format = "GTiff", datatype='INT4S', overwrite=TRUE)
        
        }else{
          cat(paste("Processed THI: ", livestock, rcp, dy, "\n"))
        }
      }
    }
}
  

