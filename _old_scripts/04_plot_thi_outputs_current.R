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
livestockLS <- c("ruminant", "broiler", "layer", "ruminant", "pig")
id <- c("NONE", "MILD", "MODERATE", "SEVERE AND DANGER")

for (livestock in livestockLS[1]){
  
  # create output folder
  oDir <- paste0(iDir, "/plots/", livestock, "/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  if (!file.exists(paste(oDir, "/historical_frequencies_", livestock, "_", ".tif", sep=""))){
    
    #load future thi in stack (all models)
    thi_freq <- stack(list.files(paste0(iDir, "/historical/frequency/", livestock, "/"), pattern = ".*_avg.*\\.tif", full.names = TRUE ))
    thi_freq <- stack(thi_freq[[3]],thi_freq[[1]],thi_freq[[2]],thi_freq[[4]]) #re-arrange layers
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
    tiff(paste(oDir, "/historical_frequencies_", livestock, ".tif", sep=""), width=3200, height=800, pointsize=8, compression='lzw',res=150)
    print(levelplot(plot, 
                    at = zvalues, 
                    scales = list(draw=FALSE), 
                    #layout=c(length(modLS),length(periodLS)), 
                    xlab="", 
                    ylab="", 
                    par.settings = myTheme, 
                    colorkey = list(space = "bottom")
    )
    + layer(sp.polygons(mask, col="grey", lwd=0.5))
    + layer(sp.polygons(wa_countries, col="black", lwd=0.5))
    )
    dev.off()
    
  }
  
  if (!file.exists(paste(oDir, "historical_trend_", livestock, ".tif", sep=""))){
    
    #load historical trend
    hist_trend <- raster(paste0(iDir, "/historical/trend/", livestock, "/severe_slope.tif"))
    hist_trend <- mask(crop(hist_trend, extent(mask)), mask)
    
    #plot settings
    plot <- setZ(hist_trend, c("historical"))
    names(plot) <- c("historical")
    zvalues <- seq(-0.02, 0.06, 0.01)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("green", "yellow", "red"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "transparent"
    myTheme$strip.background$col = "transparent"
    myTheme$axis.line$col = "transparent"
    
    #plot via levelplot
    tiff(paste(oDir, "/historical_trend_", livestock, ".tif", sep=""), width=800, height=800, pointsize=8, compression='lzw',res=150)
    print(levelplot(plot, at = zvalues, margin=FALSE, auto.key=FALSE, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom")) 
          + layer(sp.polygons(mask, col="grey", lwd=0.5))
          + layer(sp.polygons(wa_countries, col="black", lwd=0.5)))
    dev.off()
    
  }
  
  if (!file.exists(paste(oDir, "significant_trend_", livestock, ".tif", sep=""))){
    
    #load significant trend
    sign_trend <- raster(paste0(iDir, "/historical/trend/", livestock, "/severe_trend_sig.tif"))
    sign_trend <- mask(crop(sign_trend, extent(mask)), mask)
    
    #plot settings
    plot <- setZ(sign_trend, c("significant"))
    names(plot) <- c("significant")
    zvalues <- seq(0, 0.06, 0.01)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("green", "yellow", "red"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "transparent"
    myTheme$strip.background$col = "transparent"
    myTheme$axis.line$col = "transparent"
    
    #plot via levelplot
    tiff(paste(oDir, "/significant_trend_", livestock, ".tif", sep=""), width=800, height=800, pointsize=8, compression='lzw',res=150)
    print(levelplot(plot, at = zvalues, margin=FALSE, auto.key=FALSE, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom")) 
          + layer(sp.polygons(mask, col="grey", lwd=0.5))
          + layer(sp.polygons(wa_countries, col="black", lwd=0.5)))
    dev.off()
    
  }
  


}
