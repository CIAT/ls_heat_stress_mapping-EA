library(rgdal)
library(raster)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct
library(dplyr)
library(readr)


iDir <- "D:/jymutua/ls-heat-stress-mapping - EA/output/historical/cattle/frequency"
oDir <- "D:/jymutua/ls-heat-stress-mapping - EA/output/test"
p <- readOGR("D:/jymutua/ls-heat-stress-mapping - EA/output/test/test_points.shp")

var_stack <- stack(paste0(iDir, "/", "p_severe_", 1981:2010, ".tif"))

for (i in 1:nrow(p@data)){
  
  station <- p@data[i,] #extract each point
  
  coordinates(station) <- ~POINT_X+POINT_Y
  proj4string(station) <- CRS("+proj=longlat +datum=WGS84")
  
  ext <- extract(var_stack, station) #extract data
  
  rownames(ext)<-p@data$Station #name stations
  
  write.csv(ext, file = paste0(oDir, "/", i, ".csv"), row.names=TRUE)
  
}

station <- p@data[2,]
