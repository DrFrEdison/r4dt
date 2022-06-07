.xaxisdate <-  function(dayvec,type="l",formatd="day",ydata=NA,pch=20,cex=1,coltypep="red",las = 1, tz = "Europe/Berlin"){
  
  if(is.na(type)) type <- "n"
  #dayvecraw <- dayvec
  dayvecc <- as.Date(dayvec, tz = tz)
  datep <- c()
  datepp <- unique(dayvecc)
  # if(length(which(as.numeric(diff(datepp))==1))>0){
  #   # datep2 <- datepp
  #   datepp[which(as.numeric(diff(datepp))==1)+1] <- NA
  #   datepp <- datepp[which(!is.na(datepp))]
  # }
  # 
  for(i in 1:length(datepp)){
    datep[i] <- which(dayvecc==datepp[i])[1]
  }
  
  if(formatd=="cw"){ 
    datepp <- lubridate::week(datepp)
    datep <- datep[which(!duplicated(datepp))]
    datepp <- datepp[which(!duplicated(datepp))]
  }
  
  if(formatd=="time"){ 
    datepp <- dayvec
    datep <- which(!duplicated(hour(hms(strftime(dayvec, format = "%H:%M:%S", tz = tz)))))
    datepp <- datepp[which(!duplicated(hour(hms(strftime(dayvec, format = "%H:%M:%S", tz = tz)))))]
    datepp <- substr(datepp, 12, 16)
  }
  
  if(formatd=="day"){
    datepp <- datepp
    datepp <- paste(month(datepp, label = T, abbr = T), day(datepp), sep = "-")
  }
  
  if(formatd=="year-month"){
    datepp <- strftime(datepp,format="%y-%m", tz = tz)
    datep <- datep[which(!duplicated(datepp))]
    datepp <- datepp[which(!duplicated(datepp))]
  } 
  
  padj <- 0.5
  axis(1,at=datep,datepp, padj = padj, las = las, cex.axis = cex)
  axis(2)
  
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],lwd=1.2)
  if(type=="l") abline(v=datep,lty=3)
  if(type=="p") points(datep,ydata[datep],pch=pch,col=coltypep,cex=cex)
  return(datep)
}
