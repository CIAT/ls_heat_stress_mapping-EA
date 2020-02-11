#calculate THI frequency classes
#author: John Mutua, CIAT

#clear your work space
rm(list = ls(all = TRUE))

#load packages
.packages = c("rgdal","raster", "gtools", "snow")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

#set variables
iDir <- "E:/HS/future"
modLS <- c("GFDL", "HADGEM2", "MPI")
periodLS <- c("2021_2050", "2071_2100")
livestockLS <- c("ruminant","broiler", "layer", "pig") #
time_period <- c("historical", "future")
mask <- readOGR("E:/HS/shapes/mask.shp")

#load functions
source("D:/OneDrive - CGIAR/_GitHub/ls-heat-stress-mapping/scripts/00_thi_functions.R")

for (livestock in livestockLS[1]){
  
  for (mod in modLS){
    
    for (period in periodLS){
      
      #create output folder current outputs
      oDir <- paste0(iDir, "/outputs/", livestock, "/frequency/severe_classified/", sep="")
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      #load severe frequency layer
      severe_avg <- raster(paste0(iDir, "/outputs/", livestock, "/frequency/", mod, "/", period, "/p_severe_avg.tif"))
      
      #classified based of defined classes
      severe_avg_classfied <- calc(severe_avg, fun=frequency.classes)
      
      #crop layer
      severe_avg_classfied <- mask(crop(severe_avg_classfied, extent(mask)), mask)
      
      #write output
      writeRaster(severe_avg_classfied, paste0(oDir, "severe_avg_", mod, "_", period, ".tif", sep=""), format = "GTiff", datatype='INT4S', overwrite=TRUE)
      
      cat(paste("Finished classfying frequencies for: ", livestock, mod, period, "\n"))
      
    }
    
  }
    
}
  

