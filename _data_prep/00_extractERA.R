library(rgdal)
library(raster)
library(data.table)

iDir <- "D:/jymutua/ls-heat-stress-mapping - EA"
sites <- read.csv(paste0(iDir, "/data/historical/", "weather_stations.csv"), header=TRUE)
varLS <- c("HURS", "TASMAX")

varStats <- lapply(X = varLS, FUN = function(var){
  

  #gcmStats <- lapply(X=gcmLS, FUN = function(gcm){
    
    ncLS <- list.files(paste0(iDir, "/data/historical/ERA/", var, "/"), pattern = ".nc$", full.names = TRUE)
    
    ncStats <- lapply(X = ncLS, FUN = function(nc){
      
      nc <- brick(nc)
      
      nPt <- nrow(sites)
      
      shpStats <- lapply(1:nPt, FUN = function(p){
        
        shpStats <- list()
        
        site_row<- sites[p,]
        
        xy <- site_row[,c(3,2)]
        
        spdf <- SpatialPointsDataFrame(coords = xy, data = site_row, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
        
        daily_ag <- as.data.frame(t(as.data.frame(extract(nc, spdf))))
        
        nc_names <- as.data.frame(names(nc))
        
        # combine nc_names and daily_ag
        daily_ag <- cbind(nc_names, daily_ag)
        
        #rename the columns
        colnames(daily_ag) <- c("LAYER_NAME", "VAR")
        
        row.names(daily_ag) <- NULL
        
        site_id <- site_row$ID; site_long <- site_row$Long; site_lat <- site_row$Lat
        
        d <- cbind(SITE_ID=rep(site_id,times=nrow(daily_ag)), LONG=rep(site_long,times=nrow(daily_ag)), LAT=rep(site_lat, times=nrow(daily_ag)),  CL_VARIABLE=rep(var,times=nrow(daily_ag)), daily_ag)
        
        return(d)
        
      })
      
      shpStats <- do.call("rbind", shpStats)
      return(shpStats)
      
    })
    
    ncStats <- do.call(rbind, ncStats)
    return(ncStats)
    
  #})
  
  gcmtats <- do.call(rbind, gcmStats)
  return(gcmStats)

  
})

varStats.c <- do.call(rbind, varStats)

yr <- substr(varStats.c$LAYER_NAME, 2, 5); mth <- substr(varStats.c$LAYER_NAME, 7, 8); dy <- substr(varStats.c$LAYER_NAME, 10, 11)
varStats.c$YEAR <- yr; varStats.c$MONTH <- mth; varStats.c$DAY <- dy

write.csv(varStats.c, file = paste0(iDir, "/data/historical/", "weather_data_1981_2010.csv"), row.names = TRUE)
