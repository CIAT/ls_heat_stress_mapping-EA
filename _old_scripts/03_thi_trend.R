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
iDir <- "E:/HS/historical"
yrLS <- c(1981:2010)
livestockLs <- c("ruminant") #"broiler", "layer", "ruminant", "pig"

# load functions
source("D:/OneDrive - CGIAR/_GitHub/ls-heat-stress-mapping/scripts/00_thi_functions.R")

for (livestock in livestockLs){
  
  # create output folder trend outputs
  oDir <- paste0(iDir, "/outputs/", livestock, "/trend/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  #load thi layers
  thi_stack <- stack(paste0(iDir, "/outputs/", livestock, "/frequency/", "p_severe_", 1981:2010, ".tif"))
  
  #time series
  t <- 1:nlayers(thi_stack)
  
  #calculate and write slope 
  thi_slope <- calc(thi_stack, thi.slope)
  thi_slope <- thi_slope*30
  writeRaster(thi_slope, filename = paste0(oDir, "severe_slope"), format = "GTiff", overwrite=TRUE)
  
  #calculate p-value
  pvalue <- calc(thi_stack, thi.pvalue)
  writeRaster(pvalue, filename = paste0(oDir, "severe_pvalue"), format = "GTiff", overwrite=TRUE)

  #calculate significance
  pvalue_reclass <- reclassify(pvalue, thi.mat)
  p.mask.NA <- calc(pvalue_reclass, thi.sig)
  thi_trend.sig <- mask(thi_slope, p.mask.NA)
  writeRaster(thi_trend.sig, filename = paste0(oDir, "severe_trend_sig"), format = "GTiff", overwrite=TRUE)
  
  cat(paste("Processed trend: ", livestock, "\n"))
  
  
}