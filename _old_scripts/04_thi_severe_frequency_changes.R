# clear your work space
rm(list = ls(all = TRUE))

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)

# Set params
iDir <- "E:/HS"
wa_countries <- readOGR("E:/HS/shapes/WA_country_main.shp") 
mask <- readOGR("E:/HS/shapes/mask.shp")
modLS <- c("GFDL", "HADGEM2", "MPI")
periodLS <- c("2021_2050", "2071_2100")
livestockLS <- c("ruminant", "broiler", "layer", "ruminant", "pig")


#load functions
source("D:/OneDrive - CGIAR/_GitHub/ls-heat-stress-mapping/scripts/00_thi_functions.R")



for (livestock in livestockLS[1]){
  
  #load current severe layer 
  severeH <- raster(paste0(iDir, "/historical/outputs/", livestock, "/frequency/", "p_severe_avg.tif"))
  
  for (mod in modLS){
    
    for (period in periodLS){
      
      #create output folder current outputs
      oDir <- paste0(iDir, "/future/outputs/", livestock, "/change/", sep="")
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      #load future suitability in stack (all models and periods)
      severeF <- stack(paste0(iDir, "/future/outputs/", livestock, "/frequency/",  mod, "/", period, "/", "p_severe_avg.tif"))
      severeF <- resample(crop(severeF, severeH), severeH)
      
      #change calculation
      severeC <- severeF - severeH
      
      #write output
      writeRaster(severeC, paste0(oDir, "severe_change_", mod, "_", period, ".tif", sep=""), format = "GTiff", datatype='INT4S', overwrite=TRUE)
      
      cat(paste("Finished calculating change: ", livestock, mod, period, "\n"))
      
    }
    
  }
    
}



