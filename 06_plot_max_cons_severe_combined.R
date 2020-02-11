#current plots

rm(list = ls(all = TRUE))

#libraries
source("./src/requirements.R")

#trend functions
source("./src/calc_trend.R")

iDir <- "."
aoi <- readOGR(paste0(iDir, "/data/shapes/aoi.shp"))
aoi_mask <- readOGR(paste0(iDir, "/data/shapes/aoi_mask.shp"))
rcpLS <- c("RCP4.5", "RCP8.5")
periodLS <- c("2021_2050", "2071_2100")
rcpYrLs <- expand.grid("RCP"=rcpLS, "YR"=periodLS) # Combination rcp and years
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", "swine")

for (livestock in livestockLS){
  
  oDir <- paste0(iDir, "/plots/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  #average maximum consecutive severe and danger heat stress events
  if (!file.exists(paste(oDir, "avg_max_cons_severe_days_", livestock, "_", ".tif", sep=""))){
    
    #load average maximum consecutive severe
    max_cons_severeH <- raster(paste0(iDir, "/output/historical/", livestock, "/consecutive_days/", "max_cons_severe_avg_days", ".tif", sep=""))
    
    max_cons_severeF <- stack(paste0(iDir, "/output/future/", livestock, "/", rcpYrLs$RCP, "/consecutive_days/", rcpYrLs$YR, "/max_cons_severe_avg_days", ".tif", sep=""))
    
    max_cons_severeF <- resample(crop(max_cons_severeF, max_cons_severeH), max_cons_severeH)
    
    max_cons_severe_stk <- stack(max_cons_severeH, max_cons_severeF)
    
    max_cons_severe_stk <- stack(max_cons_severe_stk[[1]],max_cons_severe_stk[[2]],max_cons_severe_stk[[4]],max_cons_severe_stk[[3]], max_cons_severe_stk[[5]]) #re-arrange layers
    
    max_cons_severe_stk <- mask(crop(max_cons_severe_stk, extent(aoi_mask)), aoi_mask)
    
    max_cons_severe_class <- function(x){
      ifelse(x <= 5, 1,
             ifelse(x > 5 & x <= 10, 2, 
                    ifelse(x > 10 & x <= 20, 3, 
                           ifelse(x > 20 & x <= 30, 4, 
                                  ifelse(x > 30 & x <= 60, 5, 
                                         ifelse(x > 60, 6, NA))))))
    }
    
    max_cons_severe_stk <- calc(max_cons_severe_stk , max_cons_severe_class)
    
    id_freq<- c(paste0("Historical"), paste0(rcpLS[1], " ", "2021-2050"), paste0(rcpLS[1], " ", "2071-2100"), paste0(rcpLS[2], " ", "2021-2050"), paste0(rcpLS[2], " ", "2071-2100"))
    
    #Plot settings
    plot <- setZ(max_cons_severe_stk, id_freq)
    names(plot) <- id_freq
    zvalues <- seq(0,6,1)
    myColorkey <- list(at=zvalues, space = "bottom", height=0.95, width=1.5, labels=list(at=zvalues+0.5, 
                                                                                         labels=c("< 5 %",
                                                                                                  "5 - 10 %",
                                                                                                  "10 - 20 %",
                                                                                                  "20 - 30 %",
                                                                                                  "30 - 60 %",
                                                                                                  "> 60 %"), cex=0.8, fontfamily="serif", font=1))
    
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(brewer.pal(5, "Reds"))(length(zvalues)-1)
    myTheme$strip.border$col = "transparent"
    myTheme$axis.line$col = "black"
    stripParams <- list(cex=0.8, lines=1, col="black", fontfamily='serif', font=2)
    
    
    #plot via levelplot
    tiff(paste(oDir, "avg_max_cons_severe_days_", livestock, ".tif", sep=""), width=1600, height=500, pointsize=8, compression='lzw',res=150)
    print(levelplot(plot,
                    at = zvalues,
                    scales = list(draw=FALSE),
                    layout=c(5, 1),
                    xlab="",
                    ylab="",
                    xlim=c(21.325, 60.955),
                    ylim=c(-27.515, 18.275),
                    par.settings = myTheme,
                    par.strip.text = stripParams,
                    colorkey = myColorkey,
                    margin=FALSE)
          + latticeExtra::layer(sp.polygons(aoi_mask, col="black", lwd=1.5))
          + latticeExtra::layer(sp.polygons(aoi, col="black", lwd=1.5))
          
    )
    dev.off()
    
  }
  
}
