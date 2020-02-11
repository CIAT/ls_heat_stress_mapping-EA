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
  oDir <- paste0(iDir, "/consecutive_days/", livestock, "/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  for (yr in yrLS[6:30]){
    
    #subset dateLS by year
    yr_dates <- subset(dateLS, year == yr)
    
    #create a vector and substract month and day
    date_vec <- substr(c(yr_dates$date), 5, 8)
    
    #load thi layers
    thi_stack <- stack(paste0(iDir, "/outputs_daily_hs/", livestock, "/", "thi_", yr, date_vec, ".tif"))
    
    #everything per pixel
    max_cons_severe_days <- calc(thi_stack, function(x) max.cons.severe.days(x, 90.0))
    
    writeRaster(max_cons_severe_days, filename = paste0(oDir, "max_cons_severe_days_", yr), format = "GTiff", overwrite=TRUE)
    
    cat(paste("Processed consecutive days: ", livestock, yr, "\n"))
    
  }
  
  #means
  cons_stack <- stack(paste0(oDir, "max_cons_severe_days_", 1981:2010, ".tif")) #load thi frequency layers
  cons_mean <- mean(cons_stack, na.rm=TRUE)
  writeRaster(cons_mean, paste(oDir, "max_cons_severe_avg_days", ".tif", sep=""), overwrite=TRUE, datatype='INT4S', format="GTiff")
  
  cat(paste("Processed mean consecutive days: ", livestock, yr, "\n"))
    
}