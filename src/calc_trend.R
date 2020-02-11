#functions for calculating thi trend for different livestock species

#calculate slope 
#https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/
thi.slope=function(x){
  if (is.na(x[1])){
    NA
    }else{
      m = lm(x ~ t)
      summary(m)$coefficients[2]}
  }

#calculate significance of the trend; p-value of B1 = slope
thi.pvalue=function(x){
  if (is.na(x[1])){
    NA
    }else{
      m = lm(x ~ t)
      summary(m)$coefficients[8]}
  }

#mask all values >0.05 to get a confidence level of 95%
m <- c(0, 0.05, 1, 0.05, 1, 0)
thi.mat <- matrix(m, ncol=3, byrow=TRUE)

#check for significance
thi.sig <- function(x){
  x[x<1] <- NA
  return(x)
  }

#maximum length of consecutive severe days
max.cons.severe.days <- function(x,t){
  y <- rle((x > t)*1)
  z <- y$lengths[y$values==1]
  return(max(z,0))
}

# max.cons.severe.days <- function(x, t){
#   runs <- rle(x < t)
#   cons_days <- max(runs$lengths[runs$values==1], na.rm=TRUE)
#   return(cons_days)
# }

# p.split <- function(x){
#   ifelse(x > 0.01 & x <= 0.05, 1, 
#          ifelse(x <= 0.01, 2, NA))
# }

p.95 <- function(x){
  ifelse(x <= 0.05, 1, 0)
}

p.95check <- function(x, y){
  
  ifelse(x <= 0.05 & y <= 0.05, 1, 0)
  
}

#MannKendall test
thi.kendall <- function(x){
  return(unlist(MannKendall(x)))
}
