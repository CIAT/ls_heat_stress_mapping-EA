#convert netcdf4 files to geotiff
#author: John Mutua, CIAT

# clear your work space
rm(list = ls(all = TRUE))

#load libraries
library(raster)
library(rgdal)

#read in variables
iDir <- "D:/jymutua/HS_East_Africa"
varLS <- c("HURS","TASMAX") # 
yrLS <- c(1981:210) #
mthLS <- c(paste0("0", 1:9), 10:12)
aoi <- readOGR("D:/jymutua/HS_East_Africa/aoi_rectangle.shp") 

# #create a date dataset
# dateLS <- seq(as.Date("1981-1-1"), as.Date("2010-12-31"), by = "days")
# dateLS <- data.frame(dateLS) 
# dateLS$year <- substr(dateLS$dateLS, 1, 4); dateLS$month <- substr(dateLS$dateLS, 6, 7); dateLS$day <- substr(dateLS$dateLS, 9, 10)
# dateLS$date <- gsub("-", "", dateLS$dateLS)

#loop through all years and variables
for (var in varLS){
  
  oDir <- paste0(iDir, "/historical/data/", var)
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  #list nc files per year
  files.nc <- list.files(paste0(iDir, "/historical/", var), full.names = TRUE, recursive = TRUE)
  
  for (nc in files.nc){
    
    # #subset dateLS by year, month
    # nc_dates <- subset(dateLS, year == yr & month==mth)
    # 
    # #create a vector for use in naming the nc files
    # rs_names <- c(nc_dates$date)
    
    #stack nc file
    nc.b <- brick(nc)
    
    #names(nc.b) <- c(paste0(rs_names, coll = ""))
    
    crs(nc.b) <- CRS("+proj=longlat +datum=WGS84")
    
    nc.b <- mask(crop(nc.b, extent(aoi)), aoi)
    
    # save as GeoTIFF
    writeRaster(nc.b, filename = paste0(oDir, "/EA_"), names(nc.b), bylayer=TRUE, format = "GTiff", overwrite=TRUE)
    
  }
  
}

library(parallel)
detectCores()
