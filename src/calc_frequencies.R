# functions for calculating thi frequencies for different livestock species

# dairy cattle frequencies
# National Research Council (1971)
NormalCountDairyCattle <- function(x){sum(x <= 69.0)}
MildCountDairyCattle <- function(x){sum(x > 69.0 & x <= 75.0)} 
ModerateCountDairyCattle <- function(x){sum(x > 75.0 & x <= 85.0)}
SevereCountDairyCattle <- function(x){sum(x > 85.0)}

# Beef cattle frequencies
# Modified from National Research Council (1971)
NormalCountBeefCattle <- function(x){sum(x <= 71.0)}
MildCountBeefCattle <- function(x){sum(x > 71.0 & x <= 77.0)} 
ModerateCountBeefCattle <- function(x){sum(x > 77.0 & x <= 87.0)}
SevereCountBeefCattle <- function(x){sum(x > 87.0)}

# sheep frequencies
# LPHSI (1990) modified by Marai et al. (2001)
NormalCountSheep <- function(x){sum(x <= 25.0)}
MildCountSheep <- function(x){sum(x > 25.0 & x <= 30.0)}
ModerateCountSheep <- function(x){sum(x > 30.0 & x <= 35.0)}
SevereCountSheep <- function(x){sum(x > 35.0)}

# goat frequencies
# Modified from LPHSI (1990) modified by Marai et al. (2001)
NormalCountGoat <- function(x){sum(x <= 26.0)}
MildCountGoat <- function(x){sum(x > 26.0 & x <= 31.0)}
ModerateCountGoat <- function(x){sum(x > 31.0 & x <= 36.0)}
SevereCountGoat <- function(x){sum(x > 36.0)}

# swine frequencies
NormalCountSwine <- function(x){sum(x <= 23.0)}
MildCountSwine <- function(x){sum(x > 23.0 & x <= 26.0)}
ModerateCountSwine <- function(x){sum(x > 26.0 & x <= 28.0)}
SevereCountSwine <- function(x){sum(x > 28.0)}

# poultry frequencies
# Zulovich and DeShazer (1990)
NormalCountPoultry <- function(x){sum(x <= 27.8)}
MildCountPoultry <- function(x){sum(x > 27.8 & x <= 28.8)}
ModerateCountPoultry <- function(x){sum(x > 28.8 & x <= 29.9)}
SevereCountPoultry <- function(x){sum(x > 29.9)}

# calculate everything per pixel
categorical.mode <- function(pixel){
  mode.pixel <- DescTools::Mode(x = pixel)[1]
  return(mode.pixel)
}

#reclassify frequency
frequency.classes <- function(x){
  ifelse(x<0, 1, 
         ifelse(x >= 0 & x < 10, 2, 
                ifelse(x > 10 & x <= 20, 3, 
                       ifelse(x > 20 & x <= 30, 4, 
                              ifelse(x > 30 & x <= 40, 5, 
                                     ifelse(x > 40 & x <= 50, 6, 
                                            ifelse(x > 50 & x <= 60, 7, 
                                                   ifelse(x > 60 & x <= 70, 8, 
                                                          ifelse(x > 70 & x <= 80, 9, 
                                                                 ifelse(x > 80 & x <= 90, 10, 
                                                                        ifelse(x > 90 & x <= 100, 11, NA)))))))))))
}