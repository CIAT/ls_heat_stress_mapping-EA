#functions for calculating thi uncertainties for different livestock species

# calculate everything per pixel
categorical.mode <- function(pixel){
  mode.pixel <- DescTools::Mode(x = pixel)[1]
  return(mode.pixel)
}

agreement.pixel <- function(pixel) {
  x <- pixel[1]
  y <- pixel[2:length(pixel)]
  length(y[y == x])
}

categorical.entropy <- function(pixel){
  entropy.pixel <- DescTools::Entropy(x = table(pixel), base = exp(1))/DescTools::Entropy(x = rep(length(pixel)/length(table(pixel)), length(table(pixel))), base = exp(1))
  return(entropy.pixel)
}