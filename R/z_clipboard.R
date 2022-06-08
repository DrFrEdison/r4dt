# clipboard ####
clipboarddate <- function(date) {
  date$Date <- as.POSIXct(as.character(date$V1),tz="Europe/Berlin")
  date$day <- as.Date(date$Date,tz="Europe/Berlin")
  colp <- rainbow(length(unique(date$day)))
  colp2 <- colp[factor(date$day)]
  date$num <- c(1:length(colp2))
  date <- list(date$Date,date$day,colp2)
  names(date) <- c("Date","Day","Col")
  return(date)
}

clipboarddata <- function(data){
  clipboardsummary <- c(mean(data$V1),sd(data$V1),median(data$V1),mad(data$V1),min(data$V1),max(data$V1),var(data$V1),quantile(data$V1)[2],quantile(data$V1)[4])
  clipboardsummary <- round(clipboardsummary,2)
  data <- list(data$V1,data.frame(Parameter=c("Mean","SD","Median","Mad","Min","Max","Variance","Q1","Q3"),Data=clipboardsummary))
  names(data) <- c("raw","summary")
  return(data)
}

