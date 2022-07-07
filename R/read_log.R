read_log_files <- function( logfiles, SG = F){
  
  log <- list()
  log$dat <-  suppressWarnings(lapply(logfiles,function(x) fread(x
                                                                 , header = F
                                                                 , sep = "="
                                                                 , dec = ","
                                                                 , encoding = "Latin-1")))
  
  logfiles <- logfiles[ which( unlist( lapply( log$dat, length)) > 0 ) ]
  log$dat <- log$dat[ which( unlist( lapply( log$dat, length)) > 0 ) ]
  
  log$dat <- lapply(log$dat,function(x) x[1:(which(x$V1=="Spektrum")-1),])
  
  zerotime <- which(do.call(rbind,lapply(log$dat,function(x) ifelse(length(grep("00-00-00",x[1,2]))==0,0,1)))==1)
  log$date <- lapply(log$dat, function(x) as.POSIXct(as.character(x[1,2]),format="%Y-%m-%d_%H-%M-%S",tz="Europe/Berlin"))
  
  if(length(zerotime)>0){
    for(i in 1:length(zerotime))
      log$date[[zerotime[i]]] <- format(round(log$date[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")
  }
  
  log$dat <- lapply(log$dat,function(x) x[-1,])
  log$para <- lapply(log$dat,function(x) x$V1)
  
  log$dat <-  lapply(log$dat,function(x) x$V2 <- gsub(",",".",x$V2))
  
  log$Produktnummer <- lapply(log$para,function(x) grep("Produkt",x))
  log$Produktnummer <- mapply(function(x , y) x[y],log$dat,log$Produktnummer)
  
  if(SG){
    suppressWarnings(log$spc <- lapply(unlist(logfiles),function(x) fread(x,header=F,sep="=",dec=",",skip="Spektrum",fill=T)))
    
    log$wl <- lapply(log$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x$V2[1])),";"))))
    log$spc <- lapply(log$spc,function(x) as.numeric(unlist(strsplit(gsub(",",".",x$V1[2]),";"))))
  } else{
    suppressWarnings(log$spc <- lapply(unlist(logfiles),function(x) readLines(x)))
    
    log$wl <- lapply(log$spc, function(x) x[grep("Spektrum=", x)])
    log$spc <- lapply(log$spc, function(x) x[grep("Spektrum=", x)+1])
    
    log$wl <- lapply(log$wl, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x)),";"))))
    log$spc <- lapply(log$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",x),";"))))
  }
  
  if(length(log$dat) > 0){
    if(!SG){ log$dat <- lapply(log$dat, function(x) data.table(t(as.numeric(as.character(gsub(",", ".", x))))))
    for(i in 1:length(log$dat)) colnames(log$dat[[i]]) <- log$para[[i]]
    }
    
    if(SG){
      sgnames <- c("datetime", "date", "time", "Tank", "Produkt", "Auftrag", "Lfd. Ansatz", "Benutzer")
      log$dat1 <- mapply(function(dat, para) data.table(t(dat[ para %in% sgnames ]))
                         , dat = log$dat
                         , para = log$para
                         , SIMPLIFY = F)
      
      for(i in 1:length(log$dat1)) colnames(log$dat1[[i]]) <- log$para[[i]][log$para[[i]] %in% sgnames]
      
      log$dat2 <- mapply(function(dat, para) data.table(t(as.numeric(as.character(gsub(",", ".", dat[ !para %in% sgnames ])))))
                         , dat = log$dat
                         , para = log$para
                         , SIMPLIFY = F)
      for(i in 1:length(log$dat2)) colnames(log$dat2[[i]]) <- log$para[[i]][!log$para[[i]] %in% sgnames]
      
      log$dat <- mapply( function( x , y) data.table( x,y )
                         , x = log$dat1
                         , y = log$dat2
                         , SIMPLIFY = F)
    }
    
    log$final <- data.table(datetime = do.call(rbind,lapply(log$date,as.character))
                            , date = as.Date.character(do.call(rbind,lapply(log$date,as.character)),tz="Europe/Berlin",format="%Y-%m-%d")
                            , time = substr(do.call(rbind,lapply(log$date,as.character)),12,19)
                            , rbindlist(log$dat, fill = T)
                            , do.call(rbind, log$spc))
    
    colnames(log$final)[1:3] <- c("datetime","date","time")
    rangep <- which(colnames(log$final) == "Mittelungen") : ncol(log$final)
    suppressWarnings(colnames(log$final)[rangep][which(substr(colnames(log$final)[rangep], 1, 1) == "V" & !is.na(as.numeric(substr(colnames(log$final)[rangep], 2, 2))))] <- paste0("X", unlist(unique(log$wl))))
    
    
    
    fwrite(log$final
           , paste0(date(), "_logfile.csv")
           , sep = ";", dec = ",")
  }
}