# calculate trend  hnage
# author: John Mutua, CIAT

# clear your work space
rm(list = ls(all = TRUE))

# libraries needed
source("./src/requirements.R")

# functions to calculate thresholds
source("./src/calc_trend.R")

# set variables

iDir <- "."
periodLS <- c("2021_2050", "2041_2070", "2071_2100")
rcpLS <- c("RCP4.5", "RCP8.5")
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", "swine")
aoi_mask <- readOGR(paste0(iDir, "/data/shapes/aoi_mask.shp"))

for (livestock in livestockLS){
  
  for (rcp in rcpLS){
    
    for (period in periodLS){
      
      # create output folder current outputs
      oDir <- paste0(iDir, "/output/future/", livestock, "/", rcp, "/trend_change/", period, "/", sep="")
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      if (period == periodLS[1]) {
        
        yrLS <- c(2021:2050)
        
      } else if (period == periodLS[2]) {
        
        yrLS <- c(2041:2070)
        
      } else if (period == periodLS[3]){
        
        yrLS <- c(2071:2100)
        
      } else {
        NA
      }
      
      if (!file.exists(paste(oDir, "severe_sign_change", ".tif", sep=""))){
        
        cat(paste("Processing severe sign check: ", livestock, rcp, period, "\n"))
        
        #load historical severe pvalue layers
        sevH <- raster(paste0(iDir, "/output/historical/", livestock, "/trend/", "/severe_pvalue.tif"), sep="")
        
        #load future severe pvalue layers
        sevF <- raster(paste0(iDir, "/output/future/", livestock, "/", rcp, "/trend/", period, "/severe_pvalue.tif"), sep="")
        
        sevF <- resample(crop(sevF, sevH), sevH)
        
        severe_sign <- overlay(sevH, sevF, fun=p.95check)
        
        writeRaster(severe_sign, paste0(oDir, "severe_sign_change"), format = "GTiff", datatype='INT4S', overwrite=TRUE)
        
        
      }else{
        cat(paste("Processed severe sign check: ", livestock, rcp, period, "\n"))
      }
      
    }
    
  }
  
}
