pollutantmean<- function(directory, pollutant, id = 1:332) {
	
	files <- sprintf("%03d.csv", id)
  files <- paste(directory, files, sep='/')
	data_tables <- lapply(files, read.csv)
  
	##pollutantmean<-colClasses(data_tables)
	##pollutantmean
  
	##dt.df["nitrate"]
	##dt.df(["nitrate"],na.rm=true)
  ##mn<-mean(dt["nitrate"],na.rm=true)
  ##mn
  ##dt<-ldply(data_tables)
  x<-NULL
  for(i in data_tables){
    #z<-data_tables[[i]]
    #x<-rbind(x,z)
    dt.df<-data.frame(i)
    z<-subset(dt.df["nitrate"],!is.na(dt.df["nitrate"]))
    x<-rbind(x,z)
  }
	
  x
  mean(x[,"nitrate"])
  ##data_tables
	#dt.df<-data.frame(data_tables[[3]])
	#names(dt.df)
  
}