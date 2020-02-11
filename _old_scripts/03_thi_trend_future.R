#calculate THI slope and P-value
#author: John Mutua, CIAT

#clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("raster")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "F:/HS/future/outputs/ruminant"
modLS <- c("GFDL", "HADGEM2", "MPI")
periodLS <- c("2021_2050", "2071_2100")
livestockLS <- c("ruminant", "broiler", "layer", "ruminant", "pig")

# load functions
source("D:/OneDrive - CGIAR/_GitHub/ls-heat-stress-mapping/scripts/00_thi_functions.R")

beginCluster()

for (livestock in livestockLS[1]){
  
  for (mod in modLS){
    
    for (period in periodLS){
      
      # create output folder
      oDir <- paste0(iDir, "/trend/", mod, "/", period, "/", sep="")
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      #load frequency layers
      thi_stack <- stack(list.files(paste0(iDir, "/frequency/", mod, "/", period), pattern=glob2rx("p_severe*.tif"), full.names = TRUE))
        
      #time series
      t <- 1:nlayers(thi_stack)
        
      #calculate and write slope 
      thi_slope <- calc(thi_stack, thi.slope)
      #thi_slope <- thi_slope*nlayers(thi_stack) #trend in THI units
      writeRaster(thi_slope, filename = paste0(oDir, "severe_slope"), format = "GTiff", overwrite=TRUE)
        
      #calculate p-value
      pvalue <- calc(thi_stack, thi.pvalue)
      writeRaster(pvalue, filename = paste0(oDir, "severe_pvalue"), format = "GTiff", overwrite=TRUE)
        
      #calculate significance
      pvalue_reclass <- reclassify(pvalue, thi.mat)
      p.mask.NA <- calc(pvalue_reclass, thi.sig)
      thi_trend.sig <- mask(thi_slope, p.mask.NA)
      writeRaster(thi_trend.sig, filename = paste0(oDir, "severe_trend_sig"), format = "GTiff", overwrite=TRUE)
        
      cat(paste("Processed trend: ", livestock, mod, period, "\n"))
      
      }
  }
    
}


endCluster()