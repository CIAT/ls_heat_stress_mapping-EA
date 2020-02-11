# calculate THI slope and P-value author: John Mutua, CIAT

# clear your work space
rm(list = ls(all = TRUE))

# libraries needed
source("./src/requirements.R")

# functions to calculate thresholds
source("./src/calc_trend.R")

# set variables
iDir <- "."
yrLS <- c(1981:2010)
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", 
                 "goat", "swine")
aoi_mask <- readOGR(paste0(iDir, "/data/shapes/aoi_mask.shp"))

for (livestock in livestockLS) {
  
  # create output folder trend outputs
  oDir <- paste0(iDir, "/output/historical/", livestock, "/trend/", sep = "")
  if (!file.exists(oDir)) {
    dir.create(oDir, recursive = T)
  }
  
  # load thi layers
  thi_stack <- stack(paste0(iDir, "/output/historical/", livestock, "/frequency/", 
                            "p_severe_", 1981:2010, ".tif"))
  
  # time series
  t <- 1:nlayers(thi_stack)
  
  # calculate and write slope
  thi_slope <- calc(thi_stack, thi.slope)
  thi_slope <- thi_slope * 30
  writeRaster(thi_slope, filename = paste0(oDir, "severe_slope"), format = "GTiff", 
              overwrite = TRUE)
  
  # calculate p-value pvalue <- calc(thi_stack, thi.pvalue)
  kendall_test <- calc(thi_stack, thi.kendall)
  writeRaster(kendall_test$sl, paste0(oDir, "severe_pvalue"), format = "GTiff", 
              overwrite = TRUE)
  writeRaster(kendall_test$tau, paste0(oDir, "severe_tau"), format = "GTiff", 
              overwrite = TRUE)
  
  # # #calculate significance pvalue_reclass <- reclassify(pvalue,
  # thi.mat) p.mask.NA <- calc(pvalue_reclass, thi.sig) thi_trend.sig <-
  # mask(thi_slope, p.mask.NA) writeRaster(thi_trend.sig, filename =
  # paste0(oDir, 'severe_trend_sig'), format = 'GTiff', overwrite=TRUE)
  
  cat(paste("Processed trend: ", livestock, "\n"))
  
}