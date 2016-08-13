corr <- function(directory, threshold = 0) {
  
  filename <- list.files( path = directory )
  
  cr <- NULL
  for(i in 1:length(filename)){
    data <- read.csv( paste(directory, "/", filename[i], sep="") )
    data <- data[complete.cases(data),]
    if ( nrow(data) > threshold ) {
      cr <- c(cr, cor(data$sulfate, data$nitrate) )
    }
  }
  
  return( cr )
}