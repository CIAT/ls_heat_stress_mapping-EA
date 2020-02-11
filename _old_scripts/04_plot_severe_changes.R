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

# Combination rcp and years
modPeriodLs <- expand.grid("MODEL"=modLS, "PERIOD"=periodLS)

# Combination models and periods
id <- c("GFDL 2021_2050", "HadGEM2 2021_2050", "MPI-ESM 2021_2050", 
        "GFDL 2071_2100", "HadGEM2 2071_2100", "MPI-ESM 2071_2100")


for (livestock in livestockLS[1]){
  
  # create output folder
  oDir <- paste0(iDir, "/plots/", livestock, sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
  #load current severe layer 
  severeH <- raster(paste0(iDir, "/historical/frequency/", livestock, "/p_severe_avg.tif"))
  
  if (!file.exists(paste(oDir, "/severe_change_", livestock, ".tif", sep=""))){
    
    #load future suitability in stack (all models and periods)
    severeF <- stack(paste0(iDir, "/future/outputs/", livestock, "/frequency/",  modPeriodLs$MODEL, "/", modPeriodLs$PERIOD, "/", "p_severe_avg.tif"))
    severeF <- resample(crop(severeF, severeH), severeH)
      
    #change calculation
    severeC <- severeF - severeH
    severeC_crop <- mask(crop(severeC, extent(mask)), mask)
      
    #set limits
    severeC_crop[which(severeC_crop[]< (-15))] = (-15)
    severeC_crop[which(severeC_crop[]> 78) ] = 78
      
    #plot settings
    plot <- setZ(severeC_crop, id)
    names(plot) <- id
    zvalues <- seq(-15, 78, 10)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkgreen", "green", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
      
    # Plot via levelplot
    tiff(paste(oDir, "/severe_change_", livestock, ".tif", sep=""), width=1600, height=900, pointsize=8, compression='lzw',res=100)
    print(levelplot(plot, 
                    at = zvalues, 
                    scales = list(draw=FALSE), 
                    layout=c(length(modLS),length(periodLS)), 
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

}