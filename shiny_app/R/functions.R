## RRI Function
calculateRRI <- function(values, index) {
  
  return((abs(values$peaks[index] - values$bl) - abs(values$troughs[index] - values$bl)) / 
           abs(values$peaks[index] - values$bl))
  
}