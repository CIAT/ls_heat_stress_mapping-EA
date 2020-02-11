# calculate ensembles for future climate data
# author: John Mutua, CIAT
# last modified: 2208???2019

rm(list = ls(all = TRUE))

iDir <- "D:/jymutua/ls-heat-stress-mapping - EA"

source(paste0(iDir, "/src/requirements.R"))

varLS <- c("HURS", "TASMAX")
rcpLS <- c("RCP4.5", "RCP8.5")

dateLS <- seq(as.Date("2051-1-1"), as.Date("2070-12-31"), by = "days")
dateLS <- data.frame(dateLS) 
dateLS$year <- substr(dateLS$dateLS, 1, 4); dateLS$month <- substr(dateLS$dateLS, 6, 7); dateLS$day <- substr(dateLS$dateLS, 9, 10)
dateLS$date <- gsub("-", ".", dateLS$dateLS)
dateLS <- c(dateLS$date)

for (rcp in rcpLS){
  
  gcmLS <- list.dirs(paste0(iDir, "/data/future/_tifs/Tmax_RH_2051-2070/", rcp, "/"), recursive = FALSE, full.names = FALSE)
  
  for (var in varLS){
    
    oDir <- paste0(iDir, "/data/future/_tifs/ensembles/", rcp, "/", var, sep="")
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
    # list to be used in 40-43
    gcmFiles <- list.files(paste0(iDir, "/data/future/_tifs/Tmax_RH_2051-2070/", rcp, "/", gcmLS, "/", var), full.names = TRUE, recursive = TRUE)
    
    for (dy in dateLS){
      
      if (!file.exists(paste(oDir, "/EA__X", dy, ".tif", sep=""))){
        
        cat("Processing ensemble over :", rcp, var, dy, "\n")
        
        s_dy <- paste0("EA__X", dy) #small fix
        
        # check if file list exists
        if (!file.exists(gcmFiles[grep(s_dy, gcmFiles, fixed = TRUE)])) next
        
        # stack files
        gcmStack <- stack(gcmFiles[grep(s_dy, gcmFiles, fixed = TRUE)])
        
        # gcmStack <- stack(lapply(paste0(iDir, "/data/future/_tifs/", rcp, "/", gcmLS, "/", var, "/EA__X", dy, ".tif"),FUN=raster))
        
        gcmMean <- mean(gcmStack)
        
        writeRaster(gcmMean, filename = paste0(oDir, "/EA__X", dy, ".tif"), format = "GTiff", overwrite=TRUE)
        
      }else{
        cat(paste("Processed ensemble over :", rcp, var, dy, "\n"))
      }

    }

  }

}
