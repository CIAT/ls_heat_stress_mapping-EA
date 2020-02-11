# functions for calculating thi indices for different livestock species

# THI based on National Research Council, 1971
# x=maximum temperature; y=relative humidity
CattleThi <- function(x,y){
  x <- x-273.15 # convert kelvins to celsious
  z <- 1.8*x+32-((0.55-0.0055*y)*(1.8*x-26.8)) # cattle THI formulae
  return(z)
}

# THI small ruminants based on LPHSI (1990) modified by Marai et al. (2001)
# x=maximum temperature; y=relative humidity
SmallRuminantThi <- function(x,y){
  x <- x-273.15 # convert kelvins to celsious
  z <- x - ((0.31 - 0.31*y/100) * (x - 14.4)) # small ruminant THI formulae
  return(z)
}

# THI swine based on Marai et al. 2001
# x=maximum temperature; y=relative humidity
SwineThi <- function(x,y){
  x <- x-273.15 # convert kelvins to celsious
  b <- (x*(atan(0.151977*(y+8.313659)^0.5)))+atan(x+y)-atan(y-1.676331)+0.00391838*(y)^1.5*atan(0.023101*y)-4.686035
  z <- 0.25*b + 0.75*x # swine THI formulae
  return(z)
}

#THI poultry based on Zulovich and DeShazer, 1990
# x=maximum temperature; y=relative humidity
PoultryThi <- function(x,y){
  x <- (x-273.15) # convert kelvins celsious
  b <- (x*(atan(0.151977*(y+8.313659)^0.5)))+atan(x+y)-atan(y-1.676331)+0.00391838*(y)^1.5*atan(0.023101*y)-4.686035
  z <- x*0.6 + b*0.4 # poultry thi formulae
  return(z)
}