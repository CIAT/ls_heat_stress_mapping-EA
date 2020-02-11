#calculate THI frequency
#author: John Mutua, CIAT

#clear your work space
rm(list = ls(all = TRUE))

# load packages
.packages = c("rgdal","raster", "gtools")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

# set variables
iDir <- "F:/HS/historical"
yrLS <- c(1981:2010)
livestockLs <- c("ruminant") #"broiler", "layer", "ruminant", "pig"

#create a date dataset
dateLS <- seq(as.Date("1981-1-1"), as.Date("2010-12-31"), by = "days")
dateLS <- data.frame(dateLS) 
dateLS$year <- substr(dateLS$dateLS, 1, 4); dateLS$month <- substr(dateLS$dateLS, 6, 7); dateLS$day <- substr(dateLS$dateLS, 9, 10)
dateLS$date <- gsub("-", "", dateLS$dateLS)

# load functions
source("D:/OneDrive - CGIAR/_GitHub/ls-heat-stress-mapping/scripts/00_thi_functions.R")

#calculate THI
for (livestock in livestockLs){
  
  # create output folder current outputs
  oDir <- paste0(iDir, "/frequency/days/", livestock, "/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  for (yr in yrLS){
    
    #subset dateLS by year
    yr_dates <- subset(dateLS, year == yr)
    
    #create a vector and substract month and day
    date_vec <- substr(c(yr_dates$date), 5, 8)
    
    #load thi layers
    thi_stack <- stack(paste0(iDir, "/outputs_daily_hs/", livestock, "/", "thi_", yr, date_vec, ".tif"))
    
    # #everything per pixel
    # none_count <- calc(thi_stack, fun=none.count)
    # p_none <- none_count/(nlayers(thi_stack)+1) #empirical probability
    # writeRaster(p_none, filename = paste0(oDir, "p_none_", yr), format = "GTiff", overwrite=TRUE)
    # 
    # mild_count <- calc(thi_stack, fun=mild.count)
    # p_mild <- mild_count/(nlayers(thi_stack)+1) #empirical probability
    # writeRaster(p_mild, filename = paste0(oDir, "p_mild_", yr), format = "GTiff", overwrite=TRUE)
    # 
    # moderate_count <- calc(thi_stack, fun=moderate.count)
    # p_moderate <- moderate_count/(nlayers(thi_stack)+1) #empirical probability
    # writeRaster(p_moderate, filename = paste0(oDir, "p_moderate_", yr), format = "GTiff", overwrite=TRUE)
    
    severe_count <- calc(thi_stack, fun=severe.count)
    #p_severe <- severe_count/(nlayers(thi_stack)+1) #empirical probability
    writeRaster(severe_count, filename = paste0(oDir, "severe_days", yr), format = "GTiff", overwrite=TRUE)
    
    cat(paste("Processed frequency: ", livestock, yr, "\n"))
    
  }
  
  # #means
  # p_none_stack <- stack(paste0(oDir, "p_none_", 1981:2010, ".tif")) #load thi frequency layers
  # p_none_mean <- mean(p_none_stack, na.rm=TRUE)*100
  # writeRaster(p_none_mean, paste(oDir, "p_none_avg", ".tif", sep=""), overwrite=TRUE, format="GTiff")
  # 
  # p_mild_stack <- stack(paste0(oDir, "p_mild_", 1981:2010, ".tif")) #load thi frequency layers
  # p_mild_mean <- mean(p_mild_stack, na.rm=TRUE)*100
  # writeRaster(p_mild_mean, paste(oDir, "p_mild_avg", ".tif", sep=""), overwrite=TRUE, format="GTiff")
  # 
  # p_moderate_stack <- stack(paste0(oDir, "p_moderate_", 1981:2010, ".tif")) #load thi frequency layers
  # p_moderate_mean <- mean(p_moderate_stack, na.rm=TRUE)*100
  # writeRaster(p_moderate_mean, paste(oDir, "p_moderate_avg", ".tif", sep=""), overwrite=TRUE, format="GTiff")
  
  severe_stack <- stack(paste0(oDir, "severe_days", 1981:2010, ".tif")) #load thi frequency layers
  severe_mean <- mean(severe_stack, na.rm=TRUE)
  writeRaster(severe_mean, paste(oDir, "severe_avg_days", ".tif", sep=""), overwrite=TRUE, format="GTiff")
  
  cat(paste("Processed means: ", livestock, yr, "\n"))
    
}