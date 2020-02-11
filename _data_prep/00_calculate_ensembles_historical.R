# calculate ensembles
# author: John Mutua, CIAT
# last modified: 2208???2019

rm(list = ls(all = TRUE))

source("./src/requirements.R")

iDir <- "."
varLS <- c("HURS", "TASMAX")

dateLS <- seq(as.Date("1981-1-1"), as.Date("2010-12-31"), by = "days")
dateLS <- data.frame(dateLS) 
dateLS$year <- substr(dateLS$dateLS, 1, 4); dateLS$month <- substr(dateLS$dateLS, 6, 7); dateLS$day <- substr(dateLS$dateLS, 9, 10)
dateLS$date <- gsub("-", ".", dateLS$dateLS)
dateLS <- c(dateLS$date)


for (var in varLS[2]){
  
  gcmLS <- list.dirs(paste0(iDir, "/data/historical/GCMS_tifs/", var, "/"), recursive = FALSE, full.names = FALSE)
  
  oDir <- paste0(iDir, "/data/historical/GCMS_tifs/ensembles/", var, sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  # list to be used in 40-43
  gcmFiles <- list.files(paste0(iDir, "/data/historical/GCMS_tifs/", var), full.names = TRUE, recursive = TRUE)
  
  for (dy in dateLS){
    
    if (!file.exists(paste(oDir, "/EA__X", dy, ".tif", sep=""))){
      
      #cat("Processing ensemble over :", var, dy, "\n")
      
      s_dy <- paste0("EA__X", dy) #small fix
      
      # check if file list exists
      if (!file.exists(gcmFiles[grep(s_dy, gcmFiles, fixed = TRUE)])) next
      
      # stack files
      gcmStack <- stack(gcmFiles[grep(s_dy, gcmFiles, fixed = TRUE)])
      
      # gcmStack <- stack(lapply(paste0(iDir, "/data/future/_tifs/", rcp, "/", gcmLS, "/", var, "/EA__X", dy, ".tif"),FUN=raster))
      
      gcmMean <- mean(gcmStack)
      
      writeRaster(gcmMean, filename = paste0(oDir, "/EA__X", dy, ".tif"), format = "GTiff", overwrite=TRUE)
      
      cat(paste("Processed ensemble over :", var, dy, "\n"))
      
    }else{
      cat(paste("Processed ensemble over :", var, dy, "\n"))
    }
    
  }
  
}