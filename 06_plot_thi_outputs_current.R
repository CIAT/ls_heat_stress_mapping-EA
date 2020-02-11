#current plots

rm(list = ls(all = TRUE))

#libraries
source("./src/requirements.R")

#trend functions
source("./src/calc_trend.R")

iDir <- "."
aoi <- readOGR(paste0(iDir, "/data/shapes/aoi.shp"))
aoi_mask <- readOGR(paste0(iDir, "/data/shapes/aoi_mask.shp"))
livestockLS <- c("dairy_cattle", "beef_cattle", "poultry", "sheep", "goat", "swine")
id <- c("NONE", "MILD", "MODERATE", "SEVERE AND DANGER")

for (livestock in livestockLS){
  
  oDir <- paste0(iDir, "/plots/", sep="")
  if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
  
  #historical frequencies
  if (!file.exists(paste(oDir, "historical_frequencies_", livestock, "_", ".tif", sep=""))){
    
    #load frequency of thi categories
    thi_freq <- stack(list.files(paste0(iDir, "/output/historical/", livestock, "/frequency/"), pattern = "_avg.tif$", full.names = TRUE )) #.*_avg.*\\.tif
    thi_freq <- stack(thi_freq[[3]],thi_freq[[1]],thi_freq[[2]],thi_freq[[4]]) #re-arrange layers
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
    
    #Plot settings%
    plot <- setZ(thi_freq, id)
    names(plot) <- id
    zvalues <- seq(0,8,1)
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
    
    #plot via levelplot
    tiff(paste(oDir, "historical_frequencies_", livestock, ".tif", sep=""), width=1200, height=500, pointsize=8, compression='lzw', res=150)
    print(levelplot(plot,
                    at = zvalues,
                    scales = list(draw=FALSE),
                    layout=c(4, 1),
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
  
  # #average number of days with severe and danger heat stress events
  # if (!file.exists(paste(oDir, "historical_avg_severe_days_", livestock, "_", ".tif", sep=""))){
  # 
  #   #load frequency of severe days
  #   thi_freq.d <- raster(paste0(iDir, "/output/historical/", livestock, "/frequency/days/", "severe_avg_days", ".tif", sep=""))
  #   thi_freq.d <- mask(crop(thi_freq.d, extent(aoi_mask)), aoi_mask)
  # 
  #   #Plot settings
  #   plot <- setZ(thi_freq.d, c("historical"))
  #   names(plot) <- c("historical")
  #   zvalues <- seq(0, 365, 5)
  #   myTheme <- BuRdTheme()
  #   myTheme$regions$col=colorRampPalette(brewer.pal(length(zvalues), "YlOrRd"))
  #   myTheme$strip.border$col = "transparent"
  #   myTheme$axis.line$col = "black"
  # 
  #   #plot via levelplot
  #   tiff(paste(oDir, "historical_avg_severe_days_", livestock, ".tif", sep=""), width=800, height=1000, pointsize=8, compression='lzw',res=150)
  #   print(levelplot(plot,
  #                   at = zvalues,
  #                   margin=FALSE,
  #                   auto.key=FALSE,
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

  #average maximum consecutive severe and danger heat stress events
  if (!file.exists(paste(oDir, "historical_avg_max_cons_severe_days_", livestock, "_", ".tif", sep=""))){

    #load average maximum consecutive severe
    max_cons_severe <- raster(paste0(iDir, "/output/historical/", livestock, "/consecutive_days/", "max_cons_severe_avg_days", ".tif", sep=""))
    max_cons_severe <- mask(crop(max_cons_severe, extent(aoi_mask)), aoi_mask)
    
    max_cons_severe_class <- function(x){
      ifelse(x <= 5, 1,
             ifelse(x > 5 & x <= 10, 2, 
                    ifelse(x > 10 & x <= 20, 3, 
                           ifelse(x > 20 & x <= 30, 4, 
                                  ifelse(x > 30 & x <= 60, 5, 
                                         ifelse(x > 60, 6, NA))))))
    }
    
    max_cons_severe <- calc(max_cons_severe , max_cons_severe_class)

    #Plot settings
    plot <- setZ(max_cons_severe, c("historical"))
    names(plot) <- c("historical")
    zvalues <- seq(0,6,1)
    myColorkey <- list(at=zvalues, space = "bottom", height=0.95, width=1.5, labels=list(at=zvalues+0.5, 
                                                                                         labels=c("< 5 %",
                                                                                                  "5 - 10 %",
                                                                                                  "10 - 20 %",
                                                                                                  "20 - 30 %",
                                                                                                  "30 - 60 %",
                                                                                                  "> 60 %"), cex=0.8, fontfamily="serif", font=1))
    
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(brewer.pal(5, "YlOrBr"))(length(zvalues)-1)
    myTheme$strip.border$col = "transparent"
    myTheme$axis.line$col = "black"
    stripParams <- list(cex=0.8, lines=1, col="black", fontfamily='serif', font=2)
    
    
    #plot via levelplot
    tiff(paste(oDir, "historical_avg_max_cons_severe_days_", livestock, ".tif", sep=""), width=800, height=1000, pointsize=8, compression='lzw',res=150)
    print(levelplot(plot,
                    at = zvalues,
                    scales = list(draw=FALSE),
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
  
  # #with point overlay
  # if (!file.exists(paste(oDir, "historical_trend_sign_", livestock, ".tif", sep=""))){
  #   
  #   #load historical trend
  #   hist_trend <- raster(paste0(iDir, "/output/historical/", livestock, "/trend", "/severe_slope.tif"))
  #   hist_trend <- mask(crop(hist_trend, extent(aoi_mask)), aoi_mask)
  #   
  #   #load significant trend layer for use below
  #   severe_pvalue <- raster(paste0(iDir, "/output/historical/", livestock, "/trend", "/severe_pvalue.tif"))
  #   severe_pvalue <- mask(crop(severe_pvalue, extent(aoi_mask)), aoi_mask)
  #   
  #   #extract pixels significant at 95%
  #   severe_pvalue_95 <- calc(severe_pvalue, p.95)
  #   
  #   severe_trend_sig_shp <- rasterToPolygons(severe_pvalue_95)
  #   severe_trend_sig_shp <- aggregate(severe_trend_sig_shp)
  #   
  #   #plot settings
  #   plot <- setZ(hist_trend, c("historical"))
  #   names(plot) <- c("historical")
  #   
  #   if (livestock == "dairy_cattle") {
  #     
  #     zvalues <- seq(round(minValue(hist_trend), 2), round(maxValue(hist_trend), 2), 0.005)
  #     
  #   } else {
  #     
  #     if (livestock == "beef_cattle") {
  #       
  #       zvalues <- seq(round(minValue(hist_trend), 2), round(maxValue(hist_trend), 2), 0.005)
  #       
  #     } else {
  #       
  #       if (livestock == "poultry") {
  #         
  #         zvalues <- seq(round(minValue(hist_trend), 2), round(maxValue(hist_trend), 2), 0.005)
  #         
  #       } else {
  #         
  #         if (livestock == "sheep") {
  #           
  #           zvalues <- seq(round(minValue(hist_trend), 2), round(maxValue(hist_trend), 2), 0.005)
  #           
  #         } else {
  #           
  #           if (livestock == "goat") {
  #             
  #             zvalues <- seq(round(minValue(hist_trend), 2), round(maxValue(hist_trend), 2), 0.005)
  #             
  #           } else {
  #             
  #             if (livestock == "swine") {
  #               
  #               zvalues <- seq(round(minValue(hist_trend), 2), round(maxValue(hist_trend), 2), 0.005)
  #               
  #             } else {
  #               NA
  #             }
  #           }
  #         }
  #       }
  #     }
  #   }
  #   
  #   myTheme <- BuRdTheme()
  #   myTheme$regions$col=colorRampPalette(c("blue", "yellow", "red"))(length(zvalues)-1) # Set new colors
  #   myTheme$strip.border$col = "transparent"
  #   myTheme$axis.line$col = "black"
  #   
  #   #plot via levelplot
  #   tiff(paste(oDir, "historical_trend_sign_", livestock, ".tif", sep=""), width=800, height=800, pointsize=8, compression='lzw',res=150)
  #   print(levelplot(plot, 
  #                   at = zvalues, 
  #                   margin=FALSE, 
  #                   auto.key=FALSE, 
  #                   scales = list(draw=FALSE),  
  #                   xlab="", 
  #                   ylab="", 
  #                   xlim=c(21.325, 60.955),
  #                   ylim=c(-27.515, 18.275),
  #                   par.settings = myTheme, 
  #                   colorkey = list(space = "bottom"))
  #         + layer(sp.polygons(aoi_mask, col="black", lwd=1.5))
  #         + layer(sp.polygons(aoi, col="black", lwd=1.5))
  #         + layer(sp.polygons(severe_trend_sig_shp, density=5, angle=45, col="brown"))
  #   )
  #   dev.off()
  #   
  # }
  
}
