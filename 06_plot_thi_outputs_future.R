# future plots

rm(list = ls(all = TRUE))

# libraries needed
source("./src/requirements.R")

# trend functions
source("./src/calc_trend.R")

# Set params
iDir <- "."
rcpLS <- c("RCP4.5", "RCP8.5")
periodLS <- c("2021_2050", "2071_2100")
rcpYrLs <- expand.grid("RCP"=rcpLS, "YR"=periodLS) # Combination rcp and years
aoi <- readOGR(paste0(iDir, "/data/shapes/aoi.shp"))
aoi_mask <- readOGR(paste0(iDir, "/data/shapes/aoi_mask.shp"))
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", "swine")
#id_sev_freq<- c("RCP 4.5 2021-2050", "RCP 8.5 2021-2050", "RCP 4.5 2071-2100", "RCP 8.5 2071-2100")
# id_freq<- c("NONE 2021-2050", "MILD 2021-2050", "MODERATE 2021-2050", "SEVERE 2021-2050",
#             "NONE 2071-2100", "MILD 2071-2100", "MODERATE 2071-2100", "SEVERE 2071-2100")

for (livestock in livestockLS){
  
  # create output folder
  oDir <- paste0(iDir, "/plots/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  for (rcp in rcpLS){
    
    if (!file.exists(paste(oDir, "future_frequencies_", livestock, "_", rcp, ".tif", sep=""))){
      
      #load future frequencies (all models)
      thi_freq <- stack(list.files(paste0(iDir, "/output/future/", livestock, "/", rcp, "/frequency/", periodLS, "/"), pattern = "_avg.tif$", full.names = TRUE))
      thi_freq <- stack(thi_freq[[3]],thi_freq[[1]],thi_freq[[2]],thi_freq[[4]], thi_freq[[7]],thi_freq[[5]],thi_freq[[6]],thi_freq[[8]]) #re-arrange layers
      thi_freq <- mask(crop(thi_freq, extent(aoi_mask)), aoi_mask)
      
      thi_freq_class <- function(x){
        ifelse(x <= 2.5, 1,
               ifelse(x > 2.5 & x <= 5, 2, 
                      ifelse(x > 5 & x <= 10, 3, 
                             ifelse(x > 10 & x <= 20, 4, 
                                    ifelse(x > 20 & x <= 30, 5, 
                                           ifelse(x > 30 & x <= 40, 6, 
                                                  ifelse(x > 40 & x <= 50, 7, 
                                                         ifelse(x > 50, 8, NA))))))))
      }
      
      thi_freq <- calc(thi_freq , thi_freq_class)
      
      id_freq<- c(paste0(rcp, " ", "NONE 2021-2050"), paste0(rcp, " ", "MILD 2021-2050"), paste0(rcp, " ", "MODERATE 2021-2050"), paste0(rcp, " ", "SEVERE 2021-2050"),
                  paste0(rcp, " ", "NONE 2071-2100"), paste0(rcp, " ", "MILD 2071-2100"), paste0(rcp, " ", "MODERATE 2071-2100"), paste0(rcp, " ", "SEVERE 2071-2100"))
      
      #Plot settings%
      plot <- setZ(thi_freq, id_freq)
      names(plot) <- id_freq
      zvalues <- zvalues <- seq(0,8,1)
      myColorkey <- list(at=zvalues, space = "bottom", height=0.95, width=1.5, labels=list(at=zvalues+0.5, 
                                                                                           labels=c("0 - 2.5 %",
                                                                                                    "2.5 - 5 %",
                                                                                                    "5 - 10 %",
                                                                                                    "10 - 20 %",
                                                                                                    "20 - 30 %",
                                                                                                    "30 - 40 %",
                                                                                                    "40 - 50 %",
                                                                                                    "> 50 %"), cex=0.8, fontfamily="serif", font=1))
      
      
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(brewer.pal(7, "YlOrBr"))(length(zvalues)-1)
      myTheme$strip.border$col = "transparent"
      myTheme$axis.line$col = "black"
      stripParams <- list(cex=0.8, lines=1, col="black", fontfamily='serif', font=2)
      
      # Plot via levelplot
      tiff(paste(oDir, "future_frequencies_", livestock, "_", rcp, ".tif", sep=""), width=1600, height=1000, pointsize=8, compression='lzw',res=150)
      print(levelplot(plot,
                      at = zvalues,
                      scales = list(draw=FALSE),
                      layout=c(4, 2),
                      xlab="",
                      ylab="",
                      xlim=c(21.325, 60.955),
                      ylim=c(-27.515, 18.275),
                      par.settings = myTheme,
                      par.strip.text = stripParams,
                      colorkey = myColorkey,
                      margin=FALSE)

      + layer(sp.polygons(aoi_mask, col="black", lwd=1.5))
      + layer(sp.polygons(aoi, col="black", lwd=1.5))
      
      )
      dev.off()
      
    }
    
  }
  
  # #average number of days with severe and danger heat stress events
  # if (!file.exists(paste(oDir, "future_avg_severe_days_", livestock, ".tif", sep=""))){
  #   
  #   #load future severe frequencies
  #   thi_freq.d <- stack(paste0(iDir, "/output/future/", livestock, "/", rcpYrLs$RCP, "/frequency/", rcpYrLs$YR, "/days/", "severe_avg_days", ".tif", sep=""))
  #   thi_freq.d <- mask(crop(thi_freq.d, extent(aoi_mask)), aoi_mask)
  #   
  # 
  #   #Plot settings
  #   plot <- setZ(thi_freq.d, id_sev_freq)
  #   names(plot) <- id_sev_freq
  #   zvalues <- seq(0, 370, 5)
  #   myTheme <- BuRdTheme()
  #   myTheme$regions$col=colorRampPalette(brewer.pal(length(zvalues), "YlOrRd"))
  #   myTheme$strip.border$col = "transparent"
  #   #myTheme$strip.background$col = "transparent"
  #   myTheme$axis.line$col = "black"
  #   
  #   #plot via levelplot
  #   tiff(paste(oDir, "future_avg_severe_days_", livestock, ".tif", sep=""), width=800, height=1000, pointsize=8, compression='lzw',res=150)
  #   print(levelplot(plot, 
  #                   at = zvalues, 
  #                   margin=FALSE, 
  #                   auto.key=FALSE, 
  #                   layout=c(length(rcpLS),length(periodLS)),
  #                   scales = list(draw=FALSE),  
  #                   xlab="", 
  #                   ylab="", 
  #                   xlim=c(21.325, 60.955),
  #                   ylim=c(-27.515, 18.275),
  #                   par.settings = myTheme, 
  #                   colorkey = list(space = "bottom"))
  #         + layer(sp.polygons(aoi_mask, col="black", lwd=1.5))
  #         + layer(sp.polygons(aoi, col="black", lwd=1.5))
  #         
  #   )
  #   dev.off()
  #   
  # }
  # 
  # #average maximum consecutive severe and danger heat stress events
  # if (!file.exists(paste(oDir, "future_avg_max_cons_severe_days_", livestock, ".tif", sep=""))){
  #   
  #   #load average maximum consecutive severe
  #   max_cons_severe <- stack(paste0(iDir, "/output/future/", livestock, "/", rcpYrLs$RCP, "/consecutive_days/", rcpYrLs$YR, "/max_cons_severe_avg_days", ".tif", sep=""))
  #   max_cons_severe <- mask(crop(max_cons_severe, extent(aoi_mask)), aoi_mask)
  #   
  # 
  #   #Plot settings
  #   plot <- setZ(max_cons_severe, id_sev_freq)
  #   names(plot) <- id_sev_freq
  #   zvalues <- seq(0, 370, 5)
  #   myTheme <- BuRdTheme()
  #   myTheme$regions$col=colorRampPalette(brewer.pal(length(zvalues), "YlOrRd"))
  #   myTheme$strip.border$col = "transparent"
  #   #myTheme$strip.background$col = "transparent"
  #   myTheme$axis.line$col = "black"
  #   
  #   #plot via levelplot
  #   tiff(paste(oDir, "future_avg_max_cons_severe_days_", livestock, ".tif", sep=""), width=800, height=1000, pointsize=8, compression='lzw',res=150)
  #   
  #   print(levelplot(plot, 
  #                   at = zvalues, 
  #                   margin=FALSE, 
  #                   auto.key=FALSE,
  #                   layout=c(length(rcpLS),length(periodLS)),
  #                   scales = list(draw=FALSE),  
  #                   xlab="", 
  #                   ylab="", 
  #                   xlim=c(21.325, 60.955),
  #                   ylim=c(-27.515, 18.275),
  #                   par.settings = myTheme, 
  #                   colorkey = list(space = "bottom"))
  #         + layer(sp.polygons(aoi_mask, col="black", lwd=1.5))
  #         + layer(sp.polygons(aoi, col="black", lwd=1.5))
  #         
  #   )
  #   dev.off()
  #   
  # }
  # 
  # if (!file.exists(paste(oDir, "severe_sign_change_", livestock, ".tif", sep=""))){
  # 
  #   #load average maximum consecutive severe
  #   severe_sign_change <- stack(paste0(iDir, "/output/future/", livestock, "/", rcpYrLs$RCP, "/trend_change/", rcpYrLs$YR, "/severe_sign_change", ".tif", sep=""))
  # 
  #   severe_sign_change <- mask(crop(severe_sign_change, extent(aoi_mask)), aoi_mask)
  # 
  #   #Plot settings
  #   plot <- setZ(severe_sign_change, id_sev_freq)
  #   names(plot) <-id_sev_freq
  #   zvalues <- seq(-100, 100, 10)
  #   myTheme <- BuRdTheme()
  #   myTheme$regions$col=colorRampPalette(c("yellow", "red"))(length(zvalues)-1)
  #   myTheme$strip.border$col = "transparent"
  #   #myTheme$strip.background$col = "transparent"
  #   myTheme$axis.line$col = "black"
  # 
  #   #plot via levelplot
  #   tiff(paste(oDir, "severe_sign_change_", livestock, ".tif", sep=""), width=800, height=1000, pointsize=8, compression='lzw',res=150)
  # 
  #   print(levelplot(plot,
  #                   at = zvalues,
  #                   margin=FALSE,
  #                   auto.key=FALSE,
  #                   layout=c(length(rcpLS),length(periodLS)),
  #                   scales = list(draw=FALSE),
  #                   xlab="",
  #                   ylab="",
  #                   xlim=c(21.325, 60.955),
  #                   ylim=c(-27.515, 18.275),
  #                   par.settings = myTheme,
  #                   colorkey = list(space = "bottom"))
  #         + layer(sp.polygons(aoi_mask, col="black", lwd=1.5))
  #         + layer(sp.polygons(aoi, col="black", lwd=1.5))
  # 
  #   )
  #   dev.off()
  # 
  # }
  
}
