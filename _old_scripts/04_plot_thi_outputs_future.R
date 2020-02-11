# clear your work space
rm(list = ls(all = TRUE))

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)

# Set params
iDir <- "F:/HS"
wa_countries <- readOGR("F:/HS/shapes/WA_country_main.shp") 
mask <- readOGR("F:/HS/shapes/mask.shp")
modLS <- c("GFDL", "HADGEM2", "MPI")
periodLS <- c("2021_2050", "2071_2100")
livestockLS <- c("ruminant", "broiler", "layer", "ruminant", "pig")
thi_classes <- c("NONE", "MILD", "MODERATE", "SEVERE AND DANGER")

# Combination models and years
id <- c("GFDL NONE", "GFDL MILD", "GFDL MODERATE", "GFDL SEVERE AND DANGER",
        "HadGEM2 NONE", "HadGEM2 MILD", "HadGEM2 MODERATE", "HadGEM2 SEVERE AND DANGER",
        "MPI-ESM NONE", "MPI-ESM MILD", "MPI-ESM MODERATE", "MPI-ESM SEVERE AND DANGER")


for (livestock in livestockLS[1]){
  
  for (period in periodLS){
    
    # create output folder
    oDir <- paste0(iDir, "/plots/", livestock, sep="")
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
    if (!file.exists(paste(oDir, "/future_frequencies_", period, ".tif", sep=""))){
      
      #load future thi in stack (all years by rcp)
      thi_freq <- stack(list.files(paste0(iDir, "/future/outputs/", livestock, "/frequency/", modLS, "/", period), pattern = ".*_avg.*\\.tif", full.names = TRUE ))
      thi_freq <- stack(thi_freq[[3]],thi_freq[[1]],thi_freq[[2]],thi_freq[[4]],
                        thi_freq[[7]],thi_freq[[5]],thi_freq[[6]],thi_freq[[8]],
                        thi_freq[[11]],thi_freq[[9]],thi_freq[[10]],thi_freq[[12]]) #re-arrange layers
      thi_freq <- mask(crop(thi_freq, extent(mask)), mask)
      
      # Plot settings
      plot <- setZ(thi_freq, id)
      names(plot) <- id
      zvalues <- seq(0, 100, 5)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("darkgreen", "green", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
      myTheme$strip.border$col = "transparent"
      myTheme$strip.background$col = "transparent"
      myTheme$axis.line$col = "transparent"

      # Plot via levelplot
      tiff(paste(oDir, "/future_frequencies_", period, ".tif", sep=""), width=3200, height=1800, pointsize=8, compression='lzw',res=150)
      print(levelplot(plot, 
                      at = zvalues, 
                      scales = list(draw=FALSE), 
                      #layout=c(length(modLS),length(periodLS)), 
                      xlab="", 
                      ylab="", 
                      par.settings = myTheme, 
                      colorkey = list(space = "bottom", labels=list(cex=1, font=1.5, col="black"))
      )
      + layer(sp.polygons(mask, col="grey", lwd=0.5))
      + layer(sp.polygons(wa_countries, col="black", lwd=0.5))
      )
      dev.off()
      
      }
  }
  
}

