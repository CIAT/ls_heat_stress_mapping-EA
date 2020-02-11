library(rgdal)
library(raster)
library(data.table)

iDir <- "D:/jymutua/ls-heat-stress-mapping - EA"
sites <- read.csv(paste0(iDir, "/data/historical/", "weather_stations.csv"), header=TRUE)
varLS <- c("HURS", "TASMAX")

sites <- sites[1:80,]

varStats <- lapply(X = varLS, FUN = function(var){
  
  enLS <- list.files(paste0(iDir, "/data/historical/GCMS_tifs/ensembles/", var, "/"), pattern = ".tif$", full.names = TRUE)
  
  enStats <- lapply(X = enLS, FUN = function(en){
    
    en <- raster(en)
    
    nPt <- nrow(sites)
    
    shpStats <- lapply(1:nPt, FUN = function(p){
      
      shpStats <- list()
      
      site_row<- sites[p,]
      
      xy <- site_row[,c(3,2)]
      
      spdf <- SpatialPointsDataFrame(coords = xy, data = site_row, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      daily_ag <- as.data.frame(extract(en, spdf))
      
      #daily_ag <- as.data.frame(t(as.data.frame(extract(en, spdf))))
      
      en_names <- as.data.frame(names(en))
      
      # combine nc_names and daily_ag
      daily_ag <- cbind(en_names, daily_ag)
      
      #rename the columns
      colnames(daily_ag) <- c("LAYER_NAME", "VALUE")
      
      row.names(daily_ag) <- NULL
      
      site_id <- site_row$ID; site_long <- site_row$Long; site_lat <- site_row$Lat
      
      d <- cbind(SITE_ID=rep(site_id,times=nrow(daily_ag)), LONG=rep(site_long,times=nrow(daily_ag)), LAT=rep(site_lat, times=nrow(daily_ag)),  VARIABLE=rep(var,times=nrow(daily_ag)), daily_ag)
      
      yr <- substr(d$LAYER_NAME, 6, 9)
      mth <- substr(d$LAYER_NAME, 11, 12)
      dy <- substr(d$LAYER_NAME, 14, 15)
      d$YEAR <- yr
      d$MONTH <- mth
      d$DAY <- dy
      
      return(d)
      
    })
    
    shpStats <- do.call("rbind", shpStats)
    return(shpStats)
    
  })
  
  enStats <- do.call(rbind, enStats)
  return(enStats)
  

})

varStats <- do.call(rbind, varStats)

write.csv(enStats, file = paste0(iDir, "/data/historical/GCMS_tifs/ensembles/", "EA_1981_2010-1-80.csv"), row.names = TRUE)