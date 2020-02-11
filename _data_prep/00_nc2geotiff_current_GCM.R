#convert netcdf4 files to geotiff
#author: John Mutua, CIAT

# clear your work space
rm(list = ls(all = TRUE))

# functions needed
source("./src/requirements.R")

#read in variables
iDir <- "."
varLS <- c("HURS","TASMAX") # 
aoi <- readOGR("./data/shapes/aoi_rectangle.shp") 

for (var in varLS){
  
  # list gcms
  gcmLS <- list.dirs(paste0(iDir, "/data/historical/_netcdf/", var, "/"), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmLS){
    
    oDir <- paste0(iDir, "/data/historical/GCMS_tifs/", var, "/", gcm, "/")
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
    #list nc files per year
    files.nc <- list.files(paste0(iDir, "/data/historical/_netcdf/", var, "/", gcm), pattern = ".nc$", full.names = TRUE, recursive = TRUE)
    
    # loop through list
    for (nc in files.nc){
      
      #stack nc file
      nc.b <- brick(nc)
      
      # insert CRS
      crs(nc.b) <- CRS("+proj=longlat +datum=WGS84")
      
      #crop, mask using aoi
      nc.b <- mask(crop(nc.b, extent(aoi)), aoi)
      
      # save as GeoTIFF
      writeRaster(nc.b, filename = paste0(oDir, "/EA_"), names(nc.b), bylayer=TRUE, format = "GTiff", overwrite=TRUE)
      
    }
    
    cat(paste("Processed : ", var, gcm, "\n"))
    
  }

  
}
