#calculate THI frequency
#author: John Mutua, CIAT

#clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("rgdal","raster", "gtools", "snow")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "F:/HS/future/outputs/ruminant"
yrLS <- c(2021:2049, 2071:2099)
modLS <- c("GFDL", "HADGEM2", "MPI")
periodLS <- c("2021_2050", "2071_2100")
livestockLS <- c("ruminant", "broiler", "layer", "ruminant", "pig")

# load functions
source("D:/OneDrive - CGIAR/_GitHub/ls-heat-stress-mapping/scripts/00_thi_functions.R")

beginCluster()

#calculate frequency
for (livestock in livestockLS){
  
  for (mod in modLS){
    
    for (period in periodLS){
      
      # create output folder
      oDir <- paste0(iDir, "/frequency/", mod, "/", period, "/", sep="")
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      for (yr in yrLS){
        
        s_yr <- paste0("thi_", yr) #small fix
        
        #list all thi files
        thi_files <- list.files(paste0(iDir, "/thi/", mod, "/", period), full.names = TRUE, recursive = TRUE)
        
        #check in file list exists
        if (!file.exists(thi_files[grep(s_yr, thi_files, fixed = TRUE)])) next
        
        #stack thi file
        thi_stack<- stack(thi_files[grep(s_yr, thi_files, fixed = TRUE)])
        
        #everything per pixel
        none_count <- calc(thi_stack, fun=none.count)
        p_none <- none_count/(nlayers(thi_stack)+1) #empirical probability
        writeRaster(p_none, filename = paste0(oDir, "p_none_", yr), format = "GTiff", overwrite=TRUE)
        
        mild_count <- calc(thi_stack, fun=mild.count)
        p_mild <- mild_count/(nlayers(thi_stack)+1) #empirical probability
        writeRaster(p_mild, filename = paste0(oDir, "p_mild_", yr), format = "GTiff", overwrite=TRUE)
        
        moderate_count <- calc(thi_stack, fun=moderate.count)
        p_moderate <- moderate_count/(nlayers(thi_stack)+1) #empirical probability
        writeRaster(p_moderate, filename = paste0(oDir, "p_moderate_", yr), format = "GTiff", overwrite=TRUE)
        
        severe_count <- calc(thi_stack, fun=severe.count)
        p_severe <- severe_count/(nlayers(thi_stack)+1) #empirical probability
        writeRaster(p_severe, filename = paste0(oDir, "p_severe_", yr), format = "GTiff", overwrite=TRUE)
        
        cat(paste("Processed frequency: ", livestock, mod, period, yr, "\n"))
        
      }
      
      #means
      p_none_stack <- stack(list.files(paste0(oDir), pattern=glob2rx("p_none*.tif"), full.names = TRUE))#load thi frequency layers
      p_none_mean <- mean(p_none_stack, na.rm=TRUE)*100
      writeRaster(p_none_mean, paste(oDir, "p_none_avg", ".tif", sep=""), overwrite=TRUE, format="GTiff")
      
      p_mild_stack <- stack(list.files(paste0(oDir), pattern=glob2rx("p_mild*.tif"), full.names = TRUE)) #load thi frequency layers
      p_mild_mean <- mean(p_mild_stack, na.rm=TRUE)*100
      writeRaster(p_mild_mean, paste(oDir, "p_mild_avg", ".tif", sep=""), overwrite=TRUE, format="GTiff")
      
      p_moderate_stack <- stack(list.files(paste0(oDir), pattern=glob2rx("p_moderate*.tif"), full.names = TRUE)) #load thi frequency layers
      p_moderate_mean <- mean(p_moderate_stack, na.rm=TRUE)*100
      writeRaster(p_moderate_mean, paste(oDir, "p_moderate_avg", ".tif", sep=""), overwrite=TRUE, format="GTiff")
      
      p_severe_stack <- stack(list.files(paste0(oDir), pattern=glob2rx("p_severe*.tif"), full.names = TRUE)) #load thi frequency layers
      p_severe_mean <- mean(p_severe_stack, na.rm=TRUE)*100
      writeRaster(p_severe_mean, paste(oDir, "p_severe_avg", ".tif", sep=""), overwrite=TRUE, format="GTiff")
      
      cat(paste("Processed means: ", livestock, "means", "\n"))
      
    }
    
  }
  
}

endCluster()
