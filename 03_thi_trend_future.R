# calculate trend future
# author: John Mutua, CIAT

# clear your work space
rm(list = ls(all = TRUE))

# libraries needed
source("./src/requirements.R")

# functions to calculate thresholds
source("./src/calc_trend.R")

# set variables

iDir <- "."
periodLS <- c("2021_2050", "2041_2070", "2071_2100")
rcpLS <- c("RCP4.5", "RCP8.5")
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", "swine")
aoi_mask <- readOGR(paste0(iDir, "/data/shapes/aoi_mask.shp"))

for (livestock in livestockLS){
  
  for (rcp in rcpLS){
    
    for (period in periodLS){
      
      # create output folder current outputs
      oDir <- paste0(iDir, "/output/future/", livestock, "/", rcp, "/trend/", period, "/", sep="")
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      if (period == periodLS[1]) {
        
        yrLS <- c(2021:2050)
        
      } else if (period == periodLS[2]) {
        
        yrLS <- c(2041:2070)
        
      } else if (period == periodLS[3]){
        
        yrLS <- c(2071:2100)
        
      } else {
        NA
      }
      
      #if (!file.exists(paste(oDir, "severe_slope", ".tif", sep=""))){
      
        cat(paste("Processing trend: ", livestock, rcp, period, "\n"))
        
        #load thi layers
        thi_stack <- stack(paste0(iDir, "/output/future/", livestock, "/", rcp, "/", "frequency/", period, "/", "p_severe_", yrLS, ".tif"))
        
        #time series
        t <- 1:nlayers(thi_stack)
        
        # calculate and write slope
        thi_slope <- calc(thi_stack, thi.slope)
        thi_slope <- thi_slope*30
        writeRaster(thi_slope, filename = paste0(oDir, "severe_slope"), format = "GTiff", overwrite=TRUE)
        
        #calculate p-value
        kendall_test <- calc(thi_stack, thi.kendall)
        writeRaster(kendall_test$sl, paste0(oDir, "severe_pvalue"), format = "GTiff", overwrite=TRUE)
        writeRaster(kendall_test$tau, paste0(oDir, "severe_tau"), format = "GTiff", overwrite=TRUE)
        
        
      # }else{
      #   cat(paste("Processed trend: ", livestock, rcp, period, "\n"))
      # }
      
    }
    
  }
  
}
