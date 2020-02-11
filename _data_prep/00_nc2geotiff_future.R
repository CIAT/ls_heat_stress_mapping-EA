#convert netcdf4 files to geotiff
#author: John Mutua, CIAT

# clear your work space
rm(list = ls(all = TRUE))

#read in variables
iDir <- "D:/jymutua/ls-heat-stress-mapping - EA"
varLS <- c("HURS","TASMAX") # 
rcpLS <- c("RCP4.5", "RCP8.5")

# functions needed
source(paste0(iDir, "/src/requirements.R"))

aoi <- readOGR(paste0(iDir, "/data/shapes/aoi_rectangle.shp")) 



#loop rcp, gcms, variables
for (var in varLS){
  
  for (rcp in rcpLS){
    
    # list gcms
    gcmLS <- list.dirs(paste0(iDir, "/data/future/raw/Tmax_RH_2051-2070/", var, "/", rcp, "/"), recursive = FALSE, full.names = FALSE)
    
    for (gcm in gcmLS){
      
      oDir <- paste0(iDir, "/data/future/_tifs/Tmax_RH_2051-2070/", rcp, "/", gcm, "/", var)
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      #list nc files per year
      files.nc <- list.files(paste0(iDir, "/data/future/raw/Tmax_RH_2051-2070/", var, "/", rcp, "/", gcm), pattern = ".nc$", full.names = TRUE, recursive = TRUE)
      
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

    }
  
  }
  
  cat(paste("Processed : ", rcp, gcm, var, "\n"))
  
}
