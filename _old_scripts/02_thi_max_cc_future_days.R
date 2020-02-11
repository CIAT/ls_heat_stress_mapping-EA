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
yrLS <- c(2071:2099) #2021:2049, 
modLS <- c("GFDL", "HADGEM2", "MPI")
periodLS <- c("2021_2050", "2071_2100")
livestockLS <- c("ruminant", "broiler", "layer", "ruminant", "pig")

# load functions
source("D:/OneDrive - CGIAR/_GitHub/ls-heat-stress-mapping/scripts/00_thi_functions.R")

beginCluster()

#calculate frequency
for (livestock in livestockLS[1]){
  
  for (mod in modLS[3]){
    
    for (period in periodLS[2]){
      
      # create output folder
      oDir <- paste0(iDir, "/consecutive_days/", mod, "/", period, "/", sep="")
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      for (yr in yrLS[28:29]){
        
        s_yr <- paste0("thi_", yr) #small fix
        
        #list all thi files
        thi_files <- list.files(paste0(iDir, "/thi/", mod, "/", period), full.names = TRUE, recursive = TRUE)
        
        #check in file list exists
        if (!file.exists(thi_files[grep(s_yr, thi_files, fixed = TRUE)])) next
        
        #stack thi file
        thi_stack<- stack(thi_files[grep(s_yr, thi_files, fixed = TRUE)])
        
        # #everything per pixel
        max_cons_severe_days <- calc(thi_stack, function(x) max.cons.severe.days(x, 90.0))
        writeRaster(max_cons_severe_days, filename = paste0(oDir, "max_cons_severe_days_", yr), format = "GTiff", overwrite=TRUE)
        
        cat(paste("Processed max consecutive severe days: ", livestock, mod, period, yr, "\n"))
        
      }
      
      # #means
      cons_stack <- stack(list.files(paste0(oDir), pattern=glob2rx("max_cons_severe_days*.tif"), full.names = TRUE)) #load thi frequency layers
      cons_mean <- mean(cons_stack, na.rm=TRUE)
      writeRaster(cons_mean, paste(oDir, "max_cons_severe_avg_days", ".tif", sep=""), overwrite=TRUE, datatype='INT4S', format="GTiff")
      
      cat(paste("Processed mean max consecutive severe days: ", livestock, mod, period, "\n"))
      
    }
    
  }
  
}

endCluster()
