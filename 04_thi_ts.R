# calculate trend  hnage
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
    
    for (period in periodLS[2]){
      
      # create output folder current outputs
      oDir <- paste0(iDir, "/output/future/", livestock, "/", rcp, "/trend_significance/", period, "/", sep="")
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
      
      if (!file.exists(paste(oDir, "severe_stat_sim", ".tif", sep=""))){
        
        # load historical thi layers
        thi_stackH <- stack(paste0(iDir, "/output/historical/", livestock, "/frequency/", "p_severe_", 1981:2010, ".tif"))
        
        # load future thi layers
        thi_stackF <- stack(paste0(iDir, "/output/future/", livestock, "/", rcp, "/", "frequency/", period, "/", "p_severe_", yrLS, ".tif"))
        
        #thi_stackF <- resample(crop(thi_stackF , thi_stackH), thi_stackH)
        
        #extract coordinates for both stacks
        coordsH <- thi_stackH %>% raster::as.data.frame(xy = T, na.rm = T) %>% dplyr::select(x, y)
        coordsF <- thi_stackF %>% raster::as.data.frame(xy = T, na.rm = T) %>% dplyr::select(x, y)
        
        #check if coordinates match cell ids
        coordsH$cellID <- raster::cellFromXY(thi_stackH, coordsH)
        coordsF$cellID <- raster::cellFromXY(thi_stackF, coordsF)
        
        #extract raste values from stack
        df_H <- cbind(coordsH, raster::extract(thi_stackH, coordsH[,1:2]))
        df_F <- cbind(coordsF, raster::extract(thi_stackF, coordsF[,1:2]))
        
        # df_H <- rasterFromXYZ(df_H[, c("x", "y", "p_severe_1981")])
        # crs(df_H) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        # writeRaster(df_H, paste0(oDir, "df_H"), format = "GTiff", overwrite=TRUE)
        
        cat(paste("Processing severe statistical significance: ", livestock, rcp, period, "\n"))
        
        for (i in 1:9507) {
          x <- df_H[i, 4:33]
          y  <- df_F[i, 4:33]
          
          # if Pvalue >= 0.05, the averages of two time series are similar
          res <- t.test(x, y, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)
          
          df_H$pvalue[i] <- res$p.value

        }
        
        #replace NaNs
        df_H$pvalue <- replace(df_H$pvalue, is.na(df_H$pvalue), 0)

        df_H$stat_sim <- ifelse(df_H$pvalue >= 0.05, 1, 0) 
        
        #combine all data
        stat_sim <- df_H %>%
          select(x, y, cellID, stat_sim)
        
        severe_stat_sim <- rasterFromXYZ(stat_sim[, c("x", "y", "stat_sim")])
        
        crs(severe_stat_sim) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        
        severe_stat_sim <- mask(crop(severe_stat_sim, extent(aoi_mask)), aoi_mask)
        
        writeRaster(severe_stat_sim, paste0(oDir, "severe_stat_sim"), format = "GTiff", datatype='INT4S', overwrite=TRUE)
        
      }else{
        cat(paste("Processed severe statistical significance: ", livestock, rcp, period, "\n"))
      }
      
    }
    
  }
  
}
