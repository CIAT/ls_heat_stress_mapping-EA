# functions for calculating thi threholds for different livestock species

# cattle thresholds
# 1=normal; 2=mild; 3=moderate; 4=severe
CattleThreshold <- function(x){
  ifelse(x<=72.0, 1, 
         ifelse(x > 72.0 & x < 78.0, 2, 
                ifelse(x >= 78.0 & x < 88.0, 3, 
                       ifelse(x >= 88.0, 4, NA))))
}

# small ruminant thresholds
# 1=normal; 2=mild; 3=moderate; 4=severe
SmallRuminantThreshold <- function(x){
  ifelse(x < 25.0, 1, 
         ifelse(x >= 25.0 & x < 30.0, 2, 
                ifelse(x >= 30.0 & x < 35.0, 3, 
                       ifelse(x >= 35.0, 4, NA))))
}

# swine thresholds
# 1=normal; 2=mild; 3=moderate; 4=severe
SwineThreshold <- function(x){
  ifelse(x<=74.0, 1, 
         ifelse(x > 74.0 & x <= 78.0, 2, 
                ifelse(x > 78.0 & x < 83.0, 3, 
                       ifelse(x >=83.0, 4, NA))))
}

# poultry based on Marai et al 2001
# 1=normal; 2=mild; 3=moderate; 4=severe
PoultryThreshold <- function(x){
  ifelse(x <= 27.8, 1,
         ifelse(x > 27.8 & x < 28.8, 2,
                ifelse(x >= 28.8& x < 29.9, 3,
                       ifelse(x >= 29.9, 4, NA))))
}

# calculate difference maps
ThiChange <- function(x,y) {
  x<-x[]
  y<-y[]
  z<-0
  for(i in 1:length(x)){
    z[i] <- if(is.na(x[i])|is.na(y[i])){ 
      NA
    }else if(x[i] == 1 & y[i] == 1){ 1 #Normal to Normal
    }else if(x[i] == 1 & y[i] == 2){ 2 #Normal to Mild
    }else if(x[i] == 1 & y[i] == 3){ 3 #Normal to Alert
    }else if(x[i] == 1 & y[i] == 4){ 4 #Normal to Severe
    }else if(x[i] == 2 & y[i] == 1){ 5 #Mild to Normal
    }else if(x[i] == 2 & y[i] == 2){ 6 #Mild to Mild
    }else if(x[i] == 2 & y[i] == 3){ 7 #Mild to Alert
    }else if(x[i] == 2 & y[i] == 4){ 8 #Mild to Severe
    }else if(x[i] == 3 & y[i] == 1){ 9 #Moderate to Normal
    }else if(x[i] == 3 & y[i] == 2){ 10 #Moderate to Mild
    }else if(x[i] == 3 & y[i] == 3){ 11 #Moderate to Moderate
    }else if(x[i] == 3 & y[i] == 4){ 12 #Moderate to Severe
    }else if(x[i] == 4 & y[i] == 1){ 13 #Severe to Normal
    }else if(x[i] == 4 & y[i] == 2){ 14 #Severe to Mild
    }else if(x[i] == 4 & y[i] == 3){ 15 #Severe to Moderate
    }else if(x[i] == 4 & y[i] == 4){ 16} #Severe to Severe
    else{NA}
  }
  return(z)
}