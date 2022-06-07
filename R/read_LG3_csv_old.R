# firstday = input$firstday
# lastday = input$lastday
# location = input$location
# unit = input$unit
# product = input$product
# export_directory = input$export_directory
# fastplot = T
# typeof = input$typeof


# read_csv_files_LG3
read_csv_files_LG3 <- function(firstday, lastday,
                               location,
                               unit,
                               product = NA,
                               typecode = NA,
                               export_directory="D://csvtemp",
                               fastplot = F,
                               typeof=c("drk","ref","spc"),
                               slim = F){
  
  if(!is.na(product[1])){if(any(product[1] == 0) & location == "Mannheim") product <- "NULL"}
  
  setwd(wd$work_horse)
  
  if(slim == T) coltoremove <- c("lightPath", "ID", "transferFunctionCoef", "date", "time", "location", "unit"
                                 , "measurementTypeCode", "DTproductName"
                                 , "UNSB", "Model", "Corr", "Deviation", "scores", "sT2", "sXS", "ualsL"
                                 , "statusTimestamp", "spectrumTimestamp")
  
  # get production on each day
  
  if(!is.na(product[1])){
    produkt_files <- lapply(unique(dir( pattern = paste0(unit,".csv"))[
      c(grep(as.character(year(firstday)), dir( pattern = paste0(unit,".csv")))
        , grep(as.character(year(lastday)), dir( pattern = paste0(unit,".csv"))))
    ]), read.csv2)
    
    produkt_files <- do.call(rbind, produkt_files)
    
    produkt_files <- produkt_files[which(nchar(as.character(produkt_files$Date)) == 10),]
    
    produkt_files$Date <- as.POSIXct(produkt_files$Date)
    produkt_files <- produkt_files[which(produkt_files$Date >= firstday & produkt_files$Date <= lastday) ,]
    produkt_files$Date <- as.character(produkt_files$Date)
    
    produkt_files <- produkt_files[which(produkt_files$Produkt_1 == product),]
    
    if(!is.na(product)[1] & nrow(produkt_files) == 0) stop(paste0("No files found with product number", product), call. = F, domain = NA)
  }
  
  # Ref ####
  if(length(grep("ref",typeof))==1){
    
    setwd(wd$work_horse)
    setwd("./ref")
    ref <- list()
    
    ref$files <- dir(pattern = "_ref.csv")[which(substr(dir(pattern = "_ref.csv"), 1, 10)>=firstday & substr(dir(pattern = "_ref.csv"), 1, 10)<=lastday)]
    
    if(!is.na(product[1])){
      ref$raw <- lapply(ref$files[substr(ref$files, 1, 10) %in% produkt_files$Date] ,function(x) read.csv2(x))
      
    } else{ ref$raw <- lapply(ref$files,function(x) read.csv2(x))}
    
    ref$merge <-  do.call(rbind.fill,ref$raw)
    
    if(!is.na(product[1]) & length(product)==1) ref$merge <- ref$merge[which(ref$merge$MixerNumber==product),]
    if(!is.na(product[1]) & length(product)>1) ref$merge <- ref$merge[which(ref$merge$MixerNumber==product[1] | ref$merge$MixerNumber==product[2]),]
    
    ref$merge$date <- as.POSIXct(as.character(ref$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
    ref$merge$datetime <- as.POSIXct(as.character(ref$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin")
    
    if("ID" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "ID last")]
    if("MixerNumber" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "MixerNumber last")]
    if("transferFunctionCoef" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "transferFunctionCoef last")]
    
    if("Average" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Average last")]
    if("Light_path" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Light_path last")]
    if("IntegrationTime" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "IntegrationTime last")]
    if("TimeSinceBGD" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "TimeSinceBGD last")]
    if("Signal_ProductFlows" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Signal_ProductFlows last")]
    
    if("Flow" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Flow last")]
    if("Pressure" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Pressure last")]
    if("SPC_CaseTemp" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "SPC_CaseTemp last")]
    if("FluidTemp" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "FluidTemp last")]
    if("RackTemperature" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "RackTemperature last")]
    if("AmbientTemperature" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "AmbientTemperature last")]
    
    if("UNSB_Name" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "UNSB_Name last")]
    if("Modell" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Modell last")]
    if("Hotellingt2" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Hotellingt2 last")]
    if("Hotellingt2Lim" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Hotellingt2Lim last")]
    if("FResXSamp" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "FResXSamp last")]
    if("FResLims" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "FResLims last")]
    if("Scores" %in% names(ref$merge)) ref$merge <-  ref$merge[.moveme(names(ref$merge), "Scores last")]
    
    ref$merge <- ref$merge[.moveme(names(ref$merge), "time first")]
    ref$merge <- ref$merge[.moveme(names(ref$merge), "date first")]
    ref$merge <- ref$merge[.moveme(names(ref$merge), "datetime first")]
    
    suppressWarnings(ref$wl <- sort(as.numeric(gsub("X","",names(ref$merge)))))
    suppressWarnings(names(ref$merge)[which(!is.na(as.numeric(gsub("X","",names(ref$merge)))))] <- sort(as.numeric(gsub("X","",names(ref$merge)))))
    suppressWarnings(ref$spc <- ref$merge[,which(!is.na(as.numeric(gsub("X","",names(ref$merge)))))])
    suppressWarnings(ref$data <- ref$merge[,which(is.na(as.numeric(gsub("X","",names(ref$merge)))))])
    
    for(i in ref$wl) ref$merge <- ref$merge[.moveme(names(ref$merge), paste(i, "last"))]
    
    setwd(export_directory)
    
    if(slim == T) ref$merge <- ref$merge[ , - unique(grep(paste(coltoremove,collapse="|"), 
                                                          names(ref$merge)))[-1]]
    
    if(!is.na(product[1]) & length(product)==1) write.csv2(ref$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,gsub(" ","_",paste0("_",input$product_ID$beverage[which(input$product_ID$ID==product)])),"_",product,"_ref.csv"),row.names = F)
    if(!is.na(product[1]) & length(product)>1) write.csv2(ref$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,gsub(" ","_",paste0("_",input$product_ID$beverage[which(input$product_ID$ID==product[1])])),"_",product[1],"_",product[2],"_ref.csv"),row.names = F)
    if(is.na(product[1])) write.csv2(ref$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,"_ref.csv"),row.names = F)
    message("Reference Spectra exported")
    
    # Plot Ref ####
    if(fastplot==T){
      
      if(length(unique(ref$data$date))<=15) colp <- rainbow(length(unique(ref$data$date)))
      if(length(unique(ref$data$date))>15 & length(unique(ref$data$date))<=30) colp <- rainbow(length(unique(week(ref$data$date))))
      if(length(unique(ref$data$date))>30) colp <- rainbow(length(unique(month(ref$data$date))))
      
      if(length(unique(ref$data$date))<=15) colp2 <- colp[factor(ref$data$date)]
      if(length(unique(ref$data$date))>15 & length(unique(ref$data$date))<=30) colp2 <- colp[factor(week(ref$data$date))]
      if(length(unique(ref$data$date))>30) colp2 <- colp[factor(paste(substr(year(ref$data$date), 3, 4), formatC(month(ref$data$date), width = 2, flag = "0"), sep = "-"))]
      
      if(length(unique(ref$data$date)) <= 15) legendtext <- as.character(unique(ref$data$date))
      if(length(unique(ref$data$date)) > 15 & length(unique(ref$data$date))<=30) legendtext <- paste("KW",as.character(unique(week(ref$data$date))))
      if(length(unique(ref$data$date)) > 30) legendtext <- as.character(levels(factor(paste(substr(year(ref$data$date), 3, 4), formatC(month(ref$data$date), width = 2, flag = "0"), sep = "-"))))
      
      if(length(unique(ref$data$date)) <= 15) xaxisdate_type <- "date"
      if(length(unique(ref$data$date)) > 15 & length(unique(ref$data$date))<=30) xaxisdate_type <- "cw"
      if(length(unique(ref$data$date)) > 30) xaxisdate_type <- "year-month"
      
      png(filename=gsub(".csv",".png",filenamep),width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(4,2.5,1.5,1))
      layout(matrix(c(1, 1, 2,
                      1, 1, 3,
                      1, 1, 4,
                      5, 6, 7), nrow=4, byrow=TRUE))
      
      ylimp <- range(ref$spc,na.rm=T)
      xlimp <- range(ref$wl,na.rm=T)
      
      ifelse(!is.na(product),
             mainp <- paste0("Referenz vom ",firstday," bis ",lastday, "; ", as.character(input$product_ID$beverage[(input$product_ID$ID==product[1])]),"; ",location, " ", unit),
             mainp <- paste0("Referenz vom ",firstday," bis ",lastday, "; ",location, " ", unit))
      
      matplot(ref$wl,t(ref$spc),type="l", ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts",col=colp2,main=mainp)
      
      legend("topright",legendtext,cex=1,lty=1,col=colp,adj=0,ncol=ifelse(length(legendtext) < 21, 3, 2))
      
      spcmax <- which(ref$wl==215)
      spcmax2 <- which(ref$wl==266)
      par(mar=c(4,2.5,1.5,1))
      
      plot(1:nrow(ref$data),ref$data$integrationTime,col=colp2,pch=19,cex=.4,type="p",xlab="",ylab="",main=paste("Integrationszeit"),axes=F)
      .xaxisdate(ref$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(ref$data),ref$data$accumulations,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Mittelungen"),axes=F)
      .xaxisdate(ref$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(ref$data),as.numeric(gsub(",", ".", ref$data$FluidFlow)),col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Fluss"),axes=F)
      .xaxisdate(ref$data$date, type = "n", xaxisdate_type)
      
      ylimp <- c(quantile(ref$spc[,spcmax],na.rm=T)[2]*0.8,quantile(ref$spc[,spcmax],na.rm=T)[4]*1.2)
      plot(1:nrow(ref$data),ref$spc[,spcmax],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(ref$wl[spcmax],"nm"),axes=F,ylim=ylimp)
      .xaxisdate(ref$data$date, type = "n", xaxisdate_type)
      
      ylimp <- c(quantile(ref$spc[,spcmax2],na.rm=T)[2]*0.8,quantile(ref$spc[,spcmax2],na.rm=T)[4]*1.2)
      plot(1:nrow(ref$data),ref$spc[,spcmax2],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(ref$wl[spcmax2],"nm"),axes=F,ylim=ylimp)
      .xaxisdate(ref$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(ref$data),ref$data$FluidPressure,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Druck"),axes=F)
      .xaxisdate(ref$data$date, type = "n", xaxisdate_type)
      
      dev.off()
      message("Ref plotted")
    }
  }
  
  # drk ####
  if(length(grep("drk",typeof))==1){
    setwd(wd$work_horse)
    setwd("./drk")
    drk <- list()
    
    drk$files <- dir(pattern = "_drk.csv")[which(substr(dir(pattern = "_drk.csv"), 1, 10)>=firstday & substr(dir(pattern = "_drk.csv"), 1, 10)<=lastday)]
    
    if(!is.na(product[1])){
      drk$raw <- lapply(drk$files[substr(drk$files, 1, 10) %in% produkt_files$Date] ,function(x) read.csv2(x))
      
    } else{ drk$raw <- lapply(drk$files,function(x) read.csv2(x))}
    
    drk$merge <-  do.call(rbind.fill,drk$raw)
    
    if(!is.na(product[1]) & length(product)==1) drk$merge <- drk$merge[which(drk$merge$MixerNumber==product),]
    if(!is.na(product[1]) & length(product)>1) drk$merge <- drk$merge[which(drk$merge$MixerNumber==product[1] | drk$merge$MixerNumber==product[2]),]
    
    drk$merge$date <- as.POSIXct(as.character(drk$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
    drk$merge$datetime <- as.POSIXct(as.character(drk$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin")
    
    if("ID" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "ID last")]
    if("MixerNumber" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "MixerNumber last")]
    if("transferFunctionCoef" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "transferFunctionCoef last")]
    
    if("Average" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Average last")]
    if("Light_path" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Light_path last")]
    if("IntegrationTime" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "IntegrationTime last")]
    if("TimeSinceBGD" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "TimeSinceBGD last")]
    if("Signal_ProductFlows" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Signal_ProductFlows last")]
    
    if("Flow" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Flow last")]
    if("Pressure" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Pressure last")]
    if("SPC_CaseTemp" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "SPC_CaseTemp last")]
    if("FluidTemp" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "FluidTemp last")]
    if("RackTemperature" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "RackTemperature last")]
    if("AmbientTemperature" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "AmbientTemperature last")]
    
    if("UNSB_Name" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "UNSB_Name last")]
    if("Modell" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Modell last")]
    if("Hotellingt2" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Hotellingt2 last")]
    if("Hotellingt2Lim" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Hotellingt2Lim last")]
    if("FResXSamp" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "FResXSamp last")]
    if("FResLims" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "FResLims last")]
    if("Scores" %in% names(drk$merge)) drk$merge <-  drk$merge[.moveme(names(drk$merge), "Scores last")]
    
    drk$merge <- drk$merge[.moveme(names(drk$merge), "time first")]
    drk$merge <- drk$merge[.moveme(names(drk$merge), "date first")]
    drk$merge <- drk$merge[.moveme(names(drk$merge), "datetime first")]
    
    suppressWarnings(drk$wl <- sort(as.numeric(gsub("X","",names(drk$merge)))))
    suppressWarnings(names(drk$merge)[which(!is.na(as.numeric(gsub("X","",names(drk$merge)))))] <- sort(as.numeric(gsub("X","",names(drk$merge)))))
    suppressWarnings(drk$spc <- drk$merge[,which(!is.na(as.numeric(gsub("X","",names(drk$merge)))))])
    suppressWarnings(drk$data <- drk$merge[,which(is.na(as.numeric(gsub("X","",names(drk$merge)))))])
    
    for(i in drk$wl) drk$merge <- drk$merge[.moveme(names(drk$merge), paste(i, "last"))]
    
    setwd(export_directory)
    
    if(slim == T) drk$merge <- drk$merge[ , - unique(grep(paste(coltoremove,collapse="|"), 
                                                          names(drk$merge)))[-1]]
    
    if(!is.na(product[1]) & length(product)==1) write.csv2(drk$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,gsub(" ","_",paste0("_",input$product_ID$beverage[which(input$product_ID$ID==product)])),"_",product,"_drk.csv"),row.names = F)
    if(!is.na(product[1]) & length(product)>1) write.csv2(drk$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,gsub(" ","_",paste0("_",input$product_ID$beverage[which(input$product_ID$ID==product[1])])),"_",product[1],"_",product[2],"_drk.csv"),row.names = F)
    if(is.na(product[1])) write.csv2(drk$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,"_drk.csv"),row.names = F)
    message("drk Spectra exported")
    
    # Plot drk ####
    if(fastplot==T){
      
      if(length(unique(drk$data$date))<=15) colp <- rainbow(length(unique(drk$data$date)))
      if(length(unique(drk$data$date))>15 & length(unique(drk$data$date))<=30) colp <- rainbow(length(unique(week(drk$data$date))))
      if(length(unique(drk$data$date))>30) colp <- rainbow(length(unique(month(drk$data$date))))
      
      if(length(unique(drk$data$date))<=15) colp2 <- colp[factor(drk$data$date)]
      if(length(unique(drk$data$date))>15 & length(unique(drk$data$date))<=30) colp2 <- colp[factor(week(drk$data$date))]
      if(length(unique(drk$data$date))>30) colp2 <- colp[factor(paste(substr(year(drk$data$date), 3, 4), formatC(month(drk$data$date), width = 2, flag = "0"), sep = "-"))]
      
      if(length(unique(drk$data$date)) <= 15) legendtext <- as.character(unique(drk$data$date))
      if(length(unique(drk$data$date)) > 15 & length(unique(drk$data$date))<=30) legendtext <- paste("KW",as.character(unique(week(drk$data$date))))
      if(length(unique(drk$data$date)) > 30) legendtext <- as.character(levels(factor(paste(substr(year(drk$data$date), 3, 4), formatC(month(drk$data$date), width = 2, flag = "0"), sep = "-"))))
      
      if(length(unique(drk$data$date)) <= 15) xaxisdate_type <- "date"
      if(length(unique(drk$data$date)) > 15 & length(unique(drk$data$date))<=30) xaxisdate_type <- "cw"
      if(length(unique(drk$data$date)) > 30) xaxisdate_type <- "year-month"
      
      png(filename=gsub(".csv",".png",filenamep),width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(4,2.5,1.5,1))
      layout(matrix(c(1, 1, 2,
                      1, 1, 3,
                      1, 1, 4,
                      5, 6, 7), nrow=4, byrow=TRUE))
      ylimp <- range(drk$spc,na.rm=T)
      xlimp <- range(drk$wl,na.rm=T)
      
      ifelse(!is.na(product),
             mainp <- paste0("Dunkelwert vom ",firstday," bis ",lastday, "; ", as.character(input$product_ID$beverage[(input$product_ID$ID==product)]),"; ",location, " ", unit),
             mainp <- paste0("Dunkelwert vom ",firstday," bis ",lastday, "; ",location, " ", unit))
      
      matplot(drk$wl,t(drk$spc),type="l", ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts",col=colp2,main=mainp)
      
      legend("bottomright",legendtext,cex=1,lty=1,col=colp,adj=0,ncol=2)
      
      spcmax <- which(drk$wl==215)
      spcmax2 <- which(drk$wl==266)
      par(mar=c(4,2.5,1.5,1))
      plot(1:nrow(drk$data),drk$data$integrationTime,col=colp2,pch=19,cex=.4,type="p",xlab="",ylab="",main=paste("Integrationszeit"),axes=F)
      .xaxisdate(drk$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(drk$data),drk$data$accumulations,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Mittelungen"),axes=F)
      .xaxisdate(drk$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(drk$data),as.numeric(gsub(",", ".", drk$data$FluidFlow)),col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Fluss"),axes=F)
      .xaxisdate(drk$data$date, type = "n", xaxisdate_type)
      
      ylimp <- c(quantile(drk$spc[,spcmax],na.rm=T)[2]*0.8,quantile(drk$spc[,spcmax],na.rm=T)[4]*1.2)
      plot(1:nrow(drk$data),drk$spc[,spcmax],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(drk$wl[spcmax],"nm"),axes=F,ylim=ylimp)
      .xaxisdate(drk$data$date, type = "n", xaxisdate_type)
      
      ylimp <- c(quantile(drk$spc[,spcmax2],na.rm=T)[2]*0.8,quantile(drk$spc[,spcmax2],na.rm=T)[4]*1.2)
      plot(1:nrow(drk$data),drk$spc[,spcmax2],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(drk$wl[spcmax2],"nm"),axes=F,ylim=ylimp)
      .xaxisdate(drk$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(drk$data),drk$data$FluidPressure,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Druck"),axes=F)
      .xaxisdate(drk$data$date, type = "n", xaxisdate_type)
      
      dev.off()
      message("drk Spectra plotted")
    }
  }
  # SPC ####
  if(length(grep("spc",typeof))==1){
    setwd(wd$work_horse)
    setwd("./spc")
    spc <- list()
    
    spc$files <- dir(pattern = "_spc.csv")[which(substr(dir(pattern = "_spc.csv"), 1, 10)>=firstday & substr(dir(pattern = "_spc.csv"), 1, 10)<=lastday)]
    
    if(!is.na(product[1])){
      spc$raw <- lapply(spc$files[substr(spc$files, 1, 10) %in% produkt_files$Date] ,function(x) read.csv2(x))
      
    } else{ spc$raw <- lapply(spc$files,function(x) read.csv2(x))}
    
    productiondates <- unlist(lapply(spc$raw, function(x) unique(x$date)))
    if(!is.na(product[1])) message("Production at ", length(spc$raw), " Days")
    
    spc$merge <-  do.call(rbind.fill,spc$raw)
    
    if(!is.na(typecode) & !any(unique(spc$merge$measurementTypeCode) %in% typecode)) stop(paste0("typecode ", typecode, " not found in chosen timeframe"))
    if(!is.na(typecode)) spc$merge <- spc$merge[spc$merge$measurementTypeCode %in% typecode , ]
    
    if(!is.na(product[1]) & length(product)==1) spc$merge <- spc$merge[which(spc$merge$MixerNumber==product),]
    if(!is.na(product[1]) & length(product)>1) spc$merge <- spc$merge[which(spc$merge$MixerNumber==product[1] | spc$merge$MixerNumber==product[2]),]
    
    coltomremove <- c()
    coltomremove_NULL <- which(unlist(apply(spc$merge, 2, function(x) all(x == "NULL"))) == T)
    coltomremove_NA <- which(unlist(apply(spc$merge, 2, function(x) all(is.na(x)))) == T & unlist(apply(spc$merge, 2, function(x) length(unique(x)) == 1)) == T)
    coltomremove <- c(coltomremove_NULL, coltomremove_NA)
    
    if(length(coltomremove)>0) spc$merge <- spc$merge[,-sort(coltomremove)]
    
    spc$merge$date <- as.POSIXct(as.character(spc$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
    spc$merge$datetime <- as.POSIXct(as.character(spc$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin")
    
    suppressWarnings(numcol <-  sort(as.numeric(gsub("X","",names(spc$merge)))))
    for(i in numcol) spc$merge <- spc$merge[.moveme(names(spc$merge), paste0("X", i, " last"))]
    
    spc$merge <- spc$merge[.moveme(names(spc$merge), "time first")]
    spc$merge <- spc$merge[.moveme(names(spc$merge), "date first")]
    spc$merge <- spc$merge[.moveme(names(spc$merge), "datetime first")]
    
    suppressWarnings(spc$spc <- spc$merge[,which(!is.na(as.numeric(gsub("X","",names(spc$merge)))))])
    suppressWarnings(spc$wl <- sort(as.numeric(gsub("X","",names(spc$merge)))))
    suppressWarnings(names(spc$merge)[which(!is.na(as.numeric(gsub("X","",names(spc$merge)))))] <- sort(as.numeric(gsub("X","",names(spc$merge)))))
    
    for(i in spc$wl)  spc$merge <- spc$merge[,.moveme(colnames(spc$merge), paste(i, "last"))]
    
    if(!is.null(spc$merge$Reported_Value))suppressWarnings(spc$merge$Reported_Value <- as.numeric(gsub(",",".",as.character(spc$merge$Reported_Value))))
    if(!is.null(spc$merge$Y_Predicted_Corr))suppressWarnings(spc$merge$Y_Predicted_Corr <- as.numeric(gsub(",",".",as.character(spc$merge$Y_Predicted_Corr))))
    if(!is.null(spc$merge$Y_Predicted))suppressWarnings(spc$merge$Y_Predicted <- as.numeric(gsub(",",".",as.character(spc$merge$Y_Predicted))))
    if(!is.null(spc$merge$YDeviation))suppressWarnings(spc$merge$YDeviation <- as.numeric(gsub(",",".",as.character(spc$merge$YDeviation))))
    if(!is.null(spc$merge$Scores))suppressWarnings(spc$merge$Scores <- as.numeric(gsub(",",".",as.character(spc$merge$Scores))))
    if(!is.null(spc$merge$Hotellingt2))suppressWarnings(spc$merge$Hotellingt2 <- as.numeric(gsub(",",".",as.character(spc$merge$Hotellingt2))))
    if(!is.null(spc$merge$Hotellingt2Lim))suppressWarnings(spc$merge$Hotellingt2Lim <- as.numeric(gsub(",",".",as.character(spc$merge$Hotellingt2Lim))))
    if(!is.null(spc$merge$FResXSamp))suppressWarnings(spc$merge$FResXSamp <- as.numeric(gsub(",",".",as.character(spc$merge$FResXSamp))))
    if(!is.null(spc$merge$FResLims))suppressWarnings(spc$merge$FResLims <- as.numeric(gsub(",",".",as.character(spc$merge$FResLims))))
    if(!is.null(spc$merge$diet))suppressWarnings(spc$merge$diet <- as.numeric(gsub(",",".",as.character(spc$merge$diet))))
    
    suppressWarnings(spc$data <- spc$merge[,which(is.na(as.numeric(gsub("X","",names(spc$merge)))))])
    if(length(which(is.na(spc$data$Bias)))>0) suppressWarnings(spc$data$Bias[which(is.na(spc$data$Bias))] <- as.numeric(spc$data$Y_Predicted) - as.numeric(spc$data$Y_Predicted_Corr))
    
    setwd(export_directory)
    
    if(slim == T) spc$merge <- spc$merge[ , - unique(grep(paste(coltoremove,collapse="|"), 
                                                          names(spc$merge)))[-1]]
    
    if(!is.na(product[1]) & length(product)==1) write.csv2(spc$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,gsub(" ","_",paste0("_",input$product_ID$beverage[which(input$product_ID$ID==product)])),"_",product,"_spc.csv"),row.names = F)
    if(!is.na(product[1]) & length(product)>1) write.csv2(spc$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,gsub(" ","_",paste0("_",input$product_ID$beverage[which(input$product_ID$ID==product[1])])),"_",product[1],"_",product[2],"_spc.csv"),row.names = F)
    if(is.na(product[1])) write.csv2(spc$merge,filenamep <- paste0(gsub("-","",substr(firstday,3,nchar(firstday))),"_",gsub("-","",substr(lastday,3,nchar(lastday))),"_",location,"_",unit,"_spc.csv"),row.names = F)
    message("Product Spectra exported")
    
    # SPC Plot ####
    if(fastplot==T){
      
      if(length(unique(spc$data$date))<=15) colp <- rainbow(length(unique(spc$data$date)))
      if(length(unique(spc$data$date))>15 & length(unique(spc$data$date))<=30) colp <- rainbow(length(unique(week(spc$data$date))))
      if(length(unique(spc$data$date))>30) colp <- rainbow(length(unique(month(spc$data$date))))
      
      if(length(unique(spc$data$date))<=15) colp2 <- colp[factor(spc$data$date)]
      if(length(unique(spc$data$date))>15 & length(unique(spc$data$date))<=30) colp2 <- colp[factor(week(spc$data$date))]
      if(length(unique(spc$data$date))>30) colp2 <- colp[factor(paste(substr(year(spc$data$date), 3, 4), formatC(month(spc$data$date), width = 2, flag = "0"), sep = "-"))]
      
      if(length(unique(spc$data$date)) <= 15) legendtext <- as.character(unique(spc$data$date))
      if(length(unique(spc$data$date)) > 15 & length(unique(spc$data$date))<=30) legendtext <- paste("KW",as.character(unique(week(spc$data$date))))
      if(length(unique(spc$data$date)) > 30) legendtext <- as.character(levels(factor(paste(substr(year(spc$data$date), 3, 4), formatC(month(spc$data$date), width = 2, flag = "0"), sep = "-"))))
      
      if(length(unique(spc$data$date)) <= 15) xaxisdate_type <- "date"
      if(length(unique(spc$data$date)) > 15 & length(unique(spc$data$date))<=30) xaxisdate_type <- "cw"
      if(length(unique(spc$data$date)) > 30) xaxisdate_type <- "year-month"
      
      png(filename=gsub(".csv",".png",filenamep),width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(4,2.5,1.5,1))
      layout(matrix(c(1, 1, 2, 3,
                      1, 1, 4, 5,
                      6, 7, 8, 9,
                      10, 10, 10, 10), nrow=4, byrow=TRUE))
      ylimp <- range(spc$spc,na.rm=T)
      xlimp <- range(spc$wl,na.rm=T)
      
      ifelse(!is.na(product[i]),
             mainp <- paste0("Produktspektren vom ",firstday," bis ",lastday, "; ", as.character(input$product_ID$beverage[(input$product_ID$ID==product[i])]),"; ",location, " ", unit),
             mainp <- paste0("Produktspektren vom ",firstday," bis ",lastday, "; ",location, " ", unit))
      
      i=1
      matplot(spc$wl,apply(spc$spc[which(spc$data$date==unique(spc$data$date)[i]),],2,median),type="n", ylim=c(0,2),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts",col=colp[i],main=mainp,cex.main=.8)
      
      for(i in 1:length(unique(spc$data$date))){  
        matplot(spc$wl,apply(spc$spc[which(spc$data$date==unique(spc$data$date)[i]),],2,function(x) median(x,na.rm=T)),type="l", ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
                xlab="Lambda (nm)",ylab="Counts",col=colp[i],add=T)
        #matplot(spc$wl,apply(spc$spc[which(spc$data$date==unique(spc$data$date)[i]),],2,max),type="l",lty=2, ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
        #        xlab="Lambda (nm)",ylab="Counts",col=colp[i],add=T)
        #matplot(spc$wl,apply(spc$spc[which(spc$data$date==unique(spc$data$date)[i]),],2,min),type="l",lty=2, ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
        #        xlab="Lambda (nm)",ylab="Counts",col=colp[i],add=T)
      }
      legend("topright",legendtext,cex=1,lty=1,col=colp,adj=0,ncol=2)
      
      spcmax <- which(spc$wl==215)
      spcmax2 <- which(spc$wl==266)
      
      par(mar=c(4,2.5,1.5,1))
      plot(1:nrow(spc$data),spc$data$integrationTime,col=colp2,pch=19,cex=.4,type="p",xlab="",ylab="",main=paste("Integrationszeit"),axes=F)
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(spc$data),spc$data$accumulations,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Mittelungen"),axes=F)
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      ylimp <- c(quantile(spc$spc[,spcmax],na.rm=T)[2]*0.8,quantile(spc$spc[,spcmax],na.rm=T)[4]*1.2)
      plot(1:nrow(spc$data),spc$spc[,spcmax],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(spc$wl[spcmax],"nm"),axes=F,ylim=ylimp)
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      ylimp <- c(quantile(spc$spc[,spcmax2],na.rm=T)[2]*0.8,quantile(spc$spc[,spcmax2],na.rm=T)[4]*1.2)
      plot(1:nrow(spc$data),spc$spc[,spcmax2],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(spc$wl[spcmax2],"nm"),axes=F,ylim=ylimp)
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(spc$data),as.numeric(as.character(gsub(",",".",spc$data$FluidFlow))),col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Fluss"),axes=F)
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(spc$data),as.numeric(as.character(gsub(",",".",spc$data$FluidPressure))),col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Druck"),axes=F)
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      plot(1:nrow(spc$data),spc$data$FluidTemperature,col="red",pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(""),
           ylim = range(c(spc$data$RackTemperature,spc$data$FluidTemperature),na.rm=T)*c(1,1.2),axes=F)
      points(1:nrow(spc$data),spc$data$RackTemperature,pch=20,cex=.4,type="p",col="blue")
      
      title(expression("Temperatur " * phantom("Fluid") * "und" * phantom("Rack")), col.main = "black")
      title(expression(phantom("Temperatur ") * "Fluid" * phantom("und Rack")), col.main = "red")
      title(expression(phantom("Temperatur ") * phantom("Fluid und") * "Rack"), col.main = "blue")
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      if(length(grep("yPredictedCorr", names(spc$data))) == 1) jjj <- 1
      if(length(grep("yPredictedCorr", names(spc$data))) > 1) jjj <- which(colSums(apply(spc$data[, grep("yPredictedCorr", names(spc$data))], 2, function(x) as.numeric(as.character(x))), na.rm = T) != 0)[1]
      
      hotellim <- tail(as.numeric(as.character(gsub(",",".",spc$data[, grep("hotellingsT2Lim", names(spc$data))[jjj]]))))[5]
      plot(1:nrow(spc$data),if(sum(as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]])),na.rm=T)==0){rep(0,nrow(spc$data))} else{as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]]))}
           ,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=names(spc$data)[grep("hotellingsT2$", names(spc$data))[jjj]],axes=F
           , ylim = c(quantile(as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]])), na.rm = T)[1] * 0.75,
                      ifelse(quantile(as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]])), na.rm = T)[4]*1.25 > hotellim 
                             , qm <- quantile(as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]])), na.rm = T)[4]*1.25
                             , qm <- hotellim * 1.25))
      )
      abline( h = tail(as.numeric(as.character(gsub(",",".",spc$data[, grep("hotellingsT2Lim", names(spc$data))[jjj]]))))[5], lty = 2)
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      if(length(which(as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]]))>qm)) > 0){
        points(which(as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]]))>qm),rep(qm, length(which(as.numeric(as.character(spc$data[, grep("hotellingsT2$", names(spc$data))[jjj]]))>qm))),col="black",pch="+")
      }
      
      plot(1:nrow(spc$data),if(sum(as.numeric(as.character(spc$data[, grep("yPredictedCorr", names(spc$data))[jjj]])),na.rm=T)==0){rep(0,nrow(spc$data))} else{as.numeric(as.character(spc$data[, grep("yPredictedCorr", names(spc$data))[jjj]]))}
           ,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=names(spc$data)[grep("yPredictedCorr", names(spc$data))[jjj]],axes=F
           , ylim = c(quantile(as.numeric(as.character(spc$data[, grep("yPredictedCorr", names(spc$data))[jjj]])), na.rm = T)[2]*.75,
                      quantile(as.numeric(as.character(spc$data[, grep("yPredictedCorr", names(spc$data))[jjj]])), na.rm = T)[4]*1.25))
      abline(h=c(95,105),lty=3,col="black")
      .xaxisdate(spc$data$date, type = "n", xaxisdate_type)
      
      dev.off()
      message("SPC plotted")
    }
  }
  
  returnlist <- list()
  if(length(grep("ref",typeof))==1) returnlist$ref <- ref
  if(length(grep("drk",typeof))==1) returnlist$drk <- drk
  if(length(grep("spc",typeof))==1) returnlist$spc <- spc
  
  if(length(returnlist)>0){
    message(paste("Export of .csv files to",export_directory, "finished"))
    return(returnlist)} else{message(as.character(input$product_ID$beverage[(input$product_ID$ID==product[i])]),"(",product,")"," was not producted, so nothing is exported as .csv")}
}
