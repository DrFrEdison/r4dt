# LG2 read and merge csv files ####
read.csv.LG2 <- function(customer
                         , location
                         , unit
                         , firstday
                         , lastday
                         , product=NA
                         , export_directory="C://csvtemp"
                         , Ringkessel=T
                         , typeof = c("ref","drk","spc")
                         , slim = T
                         , return.R = F){

  oldw <- getOption("warn")
  options(warn = -1)

  # create export dir, emtpy lists and setwd ####
  dir.create(export_directory,showWarnings = F)
  setwd(paste0(wd$work_horse,"csv"))

  if("spc" %in% typeof) SPC <- list()
  if("drk" %in% typeof) DW <- list()
  if("ref" %in% typeof) REF <- list()

  if(!is.na(product)) produktname <- input$product_ID$beverage[which(input$product_ID$ID==product)]
  if(is.na(product)) produktname <- ""
  produktname <- gsub("%", "p", gsub(" ", "_", produktname))

  csvfiles <- list()

  # read csv files ####
  csvfiles$dir <- dir(pattern=".csv")
  csvfiles$prod <- gsub(".csv","",substr(csvfiles$dir,12,nchar(csvfiles$dir)))

  if(length(grep("Hand",csvfiles$prod))>0){
    csvfiles$dir <- csvfiles$dir[-grep("Hand",csvfiles$prod)]
    csvfiles$prod <- csvfiles$prod[-grep("Hand",csvfiles$prod)]
  }

  csvfiles$clear <- unlist(lapply(gregexpr("_",csvfiles$prod),function(x) x[[1]]))
  csvfiles$prod[which(csvfiles$clear>0)] <- substr(csvfiles$prod[which(csvfiles$clear>0)],1,csvfiles$clear[which(csvfiles$clear>0)]-1)

  suppressWarnings(csvfiles$dir <- csvfiles$dir[which(!is.na(as.numeric(csvfiles$prod)))])
  suppressWarnings(csvfiles$prod <- csvfiles$prod[which(!is.na(as.numeric(csvfiles$prod)))])

  csvfiles$prod <- csvfiles$prod[which(substr(csvfiles$dir,1,10)>=firstday & substr(csvfiles$dir,1,10)<=lastday)]
  csvfiles$dir <- csvfiles$dir[which(substr(csvfiles$dir,1,10)>=firstday & substr(csvfiles$dir,1,10)<=lastday)]

  if(length(which(is.na(csvfiles$dir)))>0) csvfiles$prod <- csvfiles$prod[which(!is.na(csvfiles$dir))]
  if(length(which(is.na(csvfiles$dir)))>0) csvfiles$dir <- csvfiles$dir[which(!is.na(csvfiles$dir))]

  if(!is.na(product)) csvfiles$files <- csvfiles$dir[which(csvfiles$prod==product)] else {csvfiles$files <- csvfiles$dir}

  if(length(csvfiles$files)==0) stop(paste0("No production of ", produktname," (",product,") in the chosen timeframe!"))

  csvfiles$dat <- lapply(csvfiles$files,function(x) read.csv2(x,sep="\t",dec=",",header=F,encoding = "UTF-8",skip=1))
  csvfiles$names <- mapply(function(x,y) scan(x,what="",sep="\t",y,quiet=T),x=csvfiles$files,y=lapply(csvfiles$dat,ncol))

  if(!is.list(csvfiles$names)){
    csvfiles$names1 <- list()
    for(i in 1:ncol(csvfiles$names)) csvfiles$names1[[i]] <- csvfiles$names[,i]
    csvfiles$names <- csvfiles$names1
    csvfiles$names1 <- NULL
  }

  for(i in 1:length(csvfiles$dat)) colnames(csvfiles$dat[[i]]) <- csvfiles$names[[i]]
  csvfiles$dat <- lapply(csvfiles$dat,function(x) x<-x[,1:charmatch("Ringkessel",colnames(x))])

  lapply(csvfiles$dat,function(x) if(length(which(x$Produktnummer!=product))>0) x <- x[which(x$Produktnummer==product),])
  csvfiles$rbind <- do.call(rbind.fill,csvfiles$dat)
  message(paste0("Done with reading .csv files from ",wd$work_horse),"CSV/")


  if("spc" %in% typeof){
    # SPC ####
    setwd(paste0(wd$work_horse,"CSV_SPC"))
    setwd("./spc")

    SPC$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    SPC$fileslist <- lapply(SPC$fileslist,function(x) if(!identical(x, character(0))) x else{"///NA"})
    SPC$filesdir <- lapply(SPC$fileslist, function(x) if(length(grep("///NA",x))>0) "///NA" else{x})

    if(!is.na(product)){
      SPC$fileslist <- SPC$fileslist[match(substr(csvfiles$files,1,10),substr(unlist(SPC$filesdir),1,10))]
      SPC$filesdir <- SPC$filesdir[match(substr(csvfiles$files,1,10),substr(unlist(SPC$filesdir),1,10))]
    }

    SPC$filesdir <- rmNullObs(SPC$filesdir)
    SPC$fileslist <- rmNullObs(SPC$fileslist)

    SPC$filesdir <- unique(unlist(SPC$filesdir))

    SPC$dat <-  suppressWarnings(lapply(SPC$filesdir,function(x) read.csv2(x)))

    for(i in 1:length(SPC$dat)){
      names(SPC$dat[[i]])[grep("Produktn", names(SPC$dat[[i]]))] <- "Produktnummer"
    }

    for(i in 1:length(SPC$dat)){
      find_acid <- grep("ure.",names(SPC$dat[[i]]))
      names_acid <- names(SPC$dat[[i]])[find_acid]

      names_acid1 <- substr(names_acid, 1, unlist(lapply(gregexpr("\\.", names_acid), min))-1)
      names_acid2 <- substr(names_acid, unlist(lapply(gregexpr("_", names_acid), max)), nchar(names_acid))

      names(SPC$dat[[i]])[find_acid] <- gsub("SaeureSaeure", "Saeure", gsub("\\.","",paste(names_acid1, names_acid2, sep = ".")))
    }

    if(!is.na(product)){ SPC$dat <- lapply(SPC$dat, function(x) x <- x[which(x[grep("Produkt",names(x))]==product),])}
    for(i in length(SPC$dat):1) if(nrow(SPC$dat[[i]])==0) SPC$dat[[i]] <- NULL

    SPC$dat <- lapply(SPC$dat,function(x) x <- x[, colSums(is.na(x)) != nrow(x)])

    # zerotime
    zerotime <- which(do.call(rbind,lapply(SPC$dat,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)

    SPC$datetime <- lapply(SPC$dat, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        SPC$datetime[[zerotime[i]]] <- format(round(SPC$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    SPC$Produktnummer <- lapply(SPC$dat,function(x) grep("Produkt",names(x)))
    SPC$Produktnummer <- mapply(function(x , y) x[,y],SPC$dat,SPC$Produktnummer)

    for(i in 1:length(SPC$dat)){
      isfac <- which(sapply(SPC$dat[[i]][,-c(1:3)], is.factor))+3
      if(length(isfac)>0) for(j in as.numeric(isfac)) SPC$dat[[i]][,j] <- as.numeric(as.character(SPC$dat[[i]][,j]))
    }

    SPC$datrbind <- do.call(rbind.fill,SPC$dat)

    suppressWarnings(movevec <- names(SPC$datrbind)[which(!is.na(as.numeric(gsub("X","",names(SPC$datrbind)))))])
    for(i in 1:length(movevec)){
      col_idx <- grep(movevec[i],names(SPC$datrbind))
      SPC$datrbind <- SPC$datrbind[, c((1:ncol(SPC$datrbind))[-col_idx],col_idx)]}

    if(!is.na(product)) SPC$datrbind <- SPC$datrbind[which(SPC$datrbind$Produktnummer==product),]

    SPC$hand <- NA
    if(length(which(!(SPC$datrbind$datetime %in% csvfiles$rbind$Zeitstempel) == T))>0)  SPC$hand <- SPC$datrbind[which(!(SPC$datrbind$datetime %in% csvfiles$rbind$Zeitstempel) == T),]
    SPC$merge <- merge.data.frame(csvfiles$rbind,SPC$datrbind,by.x="Zeitstempel",by.y="datetime")
    SPC$merge <- SPC$merge[colSums(!is.na(SPC$merge)) > 0]
    SPC$hand <- data.frame(SPC$hand)

    if(!is.na(Ringkessel)) SPC$merge <- SPC$merge[which(SPC$merge[,charmatch("Ringkessel",names(SPC$merge))]==Ringkessel),]
    #if(!is.na(Fueller)) SPC$merge <- SPC$merge[which(SPC$merge[,grep("Füllerstopp",names(SPC$merge))]==Fueller),]

    SPC$merge <- SPC$merge[moveme(names(SPC$merge),"time first")]
    SPC$merge <- SPC$merge[moveme(names(SPC$merge),"date first")]
    SPC$merge <- SPC$merge[moveme(names(SPC$merge),"Zeitstempel first")]
    colnames(SPC$merge)[1] <- "datetime"

    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) SPC$merge <- SPC$merge[order(SPC$merge$datetime),]

    SPC$merge <- SPC$merge[which(!duplicated(SPC$merge$datetime)),]

    if(length(grep("Produktnummer",names(SPC$merge)))>1) SPC$merge <- SPC$merge[,-grep("Produktnummer",names(SPC$merge))[2]]
    if(length(grep("Scores",names(SPC$merge)))>0) SPC$merge <- SPC$merge[,-grep("Scores",names(SPC$merge))]
    if(length(grep("Produktprofil",names(SPC$merge)))>0) SPC$merge <- SPC$merge[,-grep("Produktprofil",names(SPC$merge))]
    if(length(grep("score",names(SPC$merge)))>0) SPC$merge <- SPC$merge[,-grep("score",names(SPC$merge))]
    if(length(which(unlist(gregexpr("Lambda",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("Lambda",names(SPC$merge)))>0))]
    if(length(which(unlist(gregexpr("Lamdba",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("Lamdba",names(SPC$merge)))>0))]
    if(length(which(unlist(gregexpr("leverages",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("leverages",names(SPC$merge)))>0))]
    if(length(which(unlist(gregexpr("XValSamp",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("XValSamp",names(SPC$merge)))>0))]

    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(grep("Produktnummer",names(SPC$hand)))>1) SPC$hand <- SPC$hand[,-grep("Produktnummer",names(SPC$hand))[2]]
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(grep("Scores",names(SPC$hand)))>0) SPC$hand <- SPC$hand[,-grep("Scores",names(SPC$hand))]
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(grep("Produktprofil",names(SPC$hand)))>0) SPC$hand <- SPC$hand[,-grep("Produktprofil",names(SPC$hand))]
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(grep("score",names(SPC$hand)))>0) SPC$hand <- SPC$hand[,-grep("score",names(SPC$hand))]
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(which(unlist(gregexpr("Lambda",names(SPC$hand)))>0))>0) SPC$hand <- SPC$hand[,-c(which(unlist(gregexpr("Lambda",names(SPC$hand)))>0))]
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(which(unlist(gregexpr("Lamdba",names(SPC$hand)))>0))>0) SPC$hand <- SPC$hand[,-c(which(unlist(gregexpr("Lamdba",names(SPC$hand)))>0))]
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(which(unlist(gregexpr("leverages",names(SPC$hand)))>0))>0) SPC$hand <- SPC$hand[,-c(which(unlist(gregexpr("leverages",names(SPC$hand)))>0))]
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) if(length(which(unlist(gregexpr("XValSamp",names(SPC$hand)))>0))>0) SPC$hand <- SPC$hand[,-c(which(unlist(gregexpr("XValSamp",names(SPC$hand)))>0))]

    if(length(which(SPC$merge$Produktnummer!=product)>0)){
      message("residual wrong product in data frame, please contact Markus")
      SPC$merge <- SPC$merge[-which(SPC$merge$Produktnummer!=product),]
    }

    names(SPC$merge)[1:2] <- c("datetime","date")
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) names(SPC$hand)[1:2] <- c("datetime","date")

    message(paste0("Done with reading product spectra (.spc) files from ",wd$work_horse),"SPC/")

    names(SPC$merge) <- gsub("X","",names(SPC$merge))
    if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) names(SPC$hand) <- gsub("X","",names(SPC$hand))

    # Plot Spc ####
    if(fastplot==T){
      SPC$merge$date <- as.POSIXct(SPC$merge$date)

      if(length(unique(SPC$merge$date))<=15) colp <- rainbow(length(unique(SPC$merge$date)))
      if(length(unique(SPC$merge$date))>15 & length(unique(SPC$merge$date))<=30) colp <- rainbow(length(unique(week(SPC$merge$date))))
      if(length(unique(SPC$merge$date))>30) colp <- rainbow(length(unique(month(SPC$merge$date))))

      if(length(unique(SPC$merge$date))<=15) colp2 <- colp[factor(SPC$merge$date)]
      if(length(unique(SPC$merge$date))>15 & length(unique(SPC$merge$date))<=30) colp2 <- colp[factor(week(SPC$merge$date))]
      if(length(unique(SPC$merge$date))>30) colp2 <- colp[factor(paste(substr(year(SPC$merge$date), 3, 4), formatC(month(SPC$merge$date), width = 2, flag = "0"), sep = "-"))]

      if(length(unique(SPC$merge$date)) <= 15) legendtext <- as.character(unique(SPC$merge$date))
      if(length(unique(SPC$merge$date)) > 15 & length(unique(SPC$merge$date))<=30) legendtext <- paste("KW",as.character(unique(week(SPC$merge$date))))
      if(length(unique(SPC$merge$date)) > 30) legendtext <- as.character(levels(factor(paste(substr(year(SPC$merge$date), 3, 4), formatC(month(SPC$merge$date), width = 2, flag = "0"), sep = "-"))))

      if(length(unique(SPC$merge$date)) <= 15) xaxisdate_type <- "day"
      if(length(unique(SPC$merge$date)) > 15 & length(unique(SPC$merge$date))<=30) xaxisdate_type <- "cw"
      if(length(unique(SPC$merge$date)) > 30) xaxisdate_type <- "year-month"

      png(filename=paste0(export_directory,"/",gsub("-","",gsub(".csv",".png",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",unit,"_",produktname,"_",product,"_spc.csv")))),width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(4,2.5,1.5,1))
      layout(matrix(c(1, 1, 2, 3,
                      1, 1, 4, 5,
                      6, 6, 7, 7), nrow=3, byrow=TRUE))

      suppressWarnings(SPC$dat <- SPC$merge[, which(is.na(as.numeric(gsub("X","",names(SPC$merge)))))])
      suppressWarnings(SPC$spc <- SPC$merge[, which(!is.na(as.numeric(gsub("X","",names(SPC$merge)))))])
      suppressWarnings(SPC$wl <- as.numeric(gsub("X","",names(SPC$merge)[which(!is.na(as.numeric(gsub("X","",names(SPC$merge)))))])))

      ylimp <- range(SPC$spc,na.rm=T)
      xlimp <- range(SPC$wl,na.rm=T)

      ifelse(!is.na(product),
             mainp <- paste0("Produktspektren vom ",firstday," bis ",lastday, "; ", as.character(input$product_ID$beverage[(input$product_ID$ID==product)]),"; ",location, " ", unit),
             mainp <- paste0("Produktspektren vom ",firstday," bis ",lastday, "; ",location, " ", unit))

      i=1
      matplot(SPC$wl,apply(SPC$spc[which(SPC$merge$date==unique(SPC$merge$date)[i]),],2,median),type="n", ylim=c(0,2),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts",col=colp[i],main=mainp,cex.main=.8)

      for(i in 1:length(unique(SPC$merge$date))){
        matplot(SPC$wl,apply(SPC$spc[which(SPC$merge$date==unique(SPC$merge$date)[i]),],2,function(x) median(x,na.rm=T)),type="l", ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
                xlab="Lambda (nm)",ylab="Counts",col=colp[i],add=T)
      }

      legend("topright",legendtext,cex=1,lty=1,col=colp,adj=0,ncol = ifelse(length(legendtext) < 21, 3, 2))

      spcmax <- which(SPC$wl==215)
      spcmax2 <- which(SPC$wl==266)

      plot(1:nrow(SPC$merge),SPC$merge$Integrationszeit,col=colp2,pch=19,cex=.4,type="p",xlab="",ylab="",main=paste("Integrationszeit"),axes=F)
      xaxisdate(SPC$merge$date, type = "n", xaxisdate_type)

      plot(1:nrow(SPC$merge),SPC$merge$Mittelungen,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Mittelungen"),axes=F)
      xaxisdate(SPC$merge$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(SPC$spc[,spcmax],na.rm=T)[2]*0.6,quantile(SPC$spc[,spcmax],na.rm=T)[4]*1.4)
      plot(1:nrow(SPC$merge),SPC$spc[,spcmax],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(SPC$wl[spcmax],"nm"),axes=F,ylim=ylimp)
      xaxisdate(SPC$merge$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(SPC$spc[,spcmax2],na.rm=T)[2]*0.6,quantile(SPC$spc[,spcmax2],na.rm=T)[4]*1.4)
      plot(1:nrow(SPC$merge),SPC$spc[,spcmax2],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(SPC$wl[spcmax2],"nm"),axes=F,ylim=ylimp)
      xaxisdate(SPC$merge$date, type = "n", xaxisdate_type)

      ypred1 <- grep("ypred", names(SPC$dat))[1]
      if(!is.na(ypred1)){
        ylimp <- c(quantile(as.numeric(SPC$dat[,ypred1]),na.rm=T)[2]*0.6,quantile(as.numeric(SPC$dat[,ypred1]),na.rm=T)[4]*1.4)
        plot(1:nrow(SPC$merge),SPC$dat[,ypred1],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(names(SPC$dat)[ypred1]),axes=F,ylim=ylimp)
        xaxisdate(SPC$merge$date, type = "n", xaxisdate_type)
      }
      if(length(grep("ypred", names(SPC$dat)))>1){
        ypred2 <- grep("ypred", names(SPC$dat))[2]
        ylimp <- c(quantile(as.numeric(SPC$dat[,ypred2]),na.rm=T)[2]*0.6,quantile(as.numeric(SPC$dat[,ypred2]),na.rm=T)[4]*1.4)
        plot(1:nrow(SPC$merge),SPC$dat[,ypred2],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(names(SPC$dat)[ypred2]),axes=F,ylim=ylimp)
        xaxisdate(SPC$merge$date, type = "n", xaxisdate_type)
      }
      dev.off()
      message("SPC plotted")
    }
  }

  if("drk" %in% typeof){
    # DW ####
    setwd(paste0(wd$work_horse,"CSV_SPC"))
    setwd("./drk")

    DW$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    DW$fileslist <- lapply(DW$fileslist,function(x) if(!identical(x, character(0))) x else{"///NA"})
    DW$filesdir <- lapply(DW$fileslist, function(x) if(length(grep("///NA",x))>0) "///NA" else{x})

    if(!is.na(product)){
      DW$fileslist <- DW$fileslist[match(substr(csvfiles$files,1,10),substr(unlist(DW$filesdir),1,10))]
      DW$filesdir <- DW$filesdir[match(substr(csvfiles$files,1,10),substr(unlist(DW$filesdir),1,10))]
    }

    DW$filesdir <- rmNullObs(DW$filesdir)
    DW$fileslist <- rmNullObs(DW$fileslist)

    DW$filesdir <- unique(unlist(DW$filesdir))

    DW$dat <-  suppressWarnings(lapply(DW$filesdir,function(x) read.csv2(x)))

    # zerotime
    zerotime <- which(do.call(rbind,lapply(DW$dat,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
    DW$datetime <- lapply(DW$dat, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d_%H-%M-%S",tz="Europe/Berlin"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        DW$datetime[[zerotime[i]]] <- format(round(DW$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}


    DW$Produktnummer <- lapply(DW$dat,function(x) grep("Produkt",names(x)))
    DW$Produktnummer <- mapply(function(x , y) x[,y],DW$dat,DW$Produktnummer)
    DW$datrbind <- do.call(rbind.fill,DW$dat)
    names(DW$datrbind)[1:2] <- c("datetime","date")
    message(paste0("Done with reading drk value spectra (.spc) files from ",wd$work_horse),"SPC/")

    names(DW$datrbind) <- gsub("X","",names(DW$datrbind))


    # Plot Spc ####
    if(fastplot==T){
      DW$datrbind$date <- as.POSIXct(DW$datrbind$date)

      if(length(unique(DW$datrbind$date))<=15) colp <- rainbow(length(unique(DW$datrbind$date)))
      if(length(unique(DW$datrbind$date))>15 & length(unique(DW$datrbind$date))<=30) colp <- rainbow(length(unique(week(DW$datrbind$date))))
      if(length(unique(DW$datrbind$date))>30) colp <- rainbow(length(unique(month(DW$datrbind$date))))

      if(length(unique(DW$datrbind$date))<=15) colp2 <- colp[factor(DW$datrbind$date)]
      if(length(unique(DW$datrbind$date))>15 & length(unique(DW$datrbind$date))<=30) colp2 <- colp[factor(week(DW$datrbind$date))]
      if(length(unique(DW$datrbind$date))>30) colp2 <- colp[factor(paste(substr(year(DW$datrbind$date), 3, 4), formatC(month(DW$datrbind$date), width = 2, flag = "0"), sep = "-"))]

      if(length(unique(DW$datrbind$date)) <= 15) legendtext <- as.character(unique(DW$datrbind$date))
      if(length(unique(DW$datrbind$date)) > 15 & length(unique(DW$datrbind$date))<=30) legendtext <- paste("KW",as.character(unique(week(DW$datrbind$date))))
      if(length(unique(DW$datrbind$date)) > 30) legendtext <- as.character(levels(factor(paste(substr(year(DW$datrbind$date), 3, 4), formatC(month(DW$datrbind$date), width = 2, flag = "0"), sep = "-"))))

      if(length(unique(DW$datrbind$date)) <= 15) xaxisdate_type <- "day"
      if(length(unique(DW$datrbind$date)) > 15 & length(unique(DW$datrbind$date))<=30) xaxisdate_type <- "cw"
      if(length(unique(DW$datrbind$date)) > 30) xaxisdate_type <- "year-month"

      png(filename=paste0(export_directory,"/",gsub("-","",gsub(".csv",".png",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",unit,"_",produktname,"_",product,"_drk.csv")))),width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(4,2.5,1.5,1))
      layout(matrix(c(1, 1, 2,
                      1, 1, 3,
                      4, 5, 6), nrow=3, byrow=TRUE))
      suppressWarnings(DW$dat <- DW$datrbind[, which(is.na(as.numeric(gsub("X","",names(DW$datrbind)))))])
      suppressWarnings(DW$spc <- DW$datrbind[, which(!is.na(as.numeric(gsub("X","",names(DW$datrbind)))))])
      suppressWarnings(DW$wl <- as.numeric(gsub("X","",names(DW$datrbind)[which(!is.na(as.numeric(gsub("X","",names(DW$datrbind)))))])))

      ylimp <- range(DW$spc,na.rm=T)
      xlimp <- range(DW$wl,na.rm=T)

      ifelse(!is.na(product),
             mainp <- paste0("Dunkelwert vom ",firstday," bis ",lastday, "; ", as.character(input$product_ID$beverage[(input$product_ID$ID==product)]),"; ",location, " ", unit),
             mainp <- paste0("Dunkelwert vom ",firstday," bis ",lastday, "; ",location, " ", unit))

      matplot(DW$wl,t(DW$spc),type="l", ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts",col=colp2,main=mainp)
      legend("topright",legendtext,cex=1,lty=1,col=colp,adj=0,ncol = ifelse(length(legendtext) < 21, 3, 2))

      spcmax <- which(DW$wl==215)
      spcmax2 <- which(DW$wl==266)
      spcmax3 <- which(DW$wl==485)

      plot(1:nrow(DW$datrbind),DW$datrbind$Integrationszeit,col=colp2,pch=19,cex=.4,type="p",xlab="",ylab="",main=paste("Integrationszeit"),axes=F)
      xaxisdate(DW$datrbind$date, type = "n", xaxisdate_type)

      plot(1:nrow(DW$datrbind),DW$datrbind$Mittelungen,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Mittelungen"),axes=F)
      xaxisdate(DW$datrbind$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(DW$spc[,spcmax],na.rm=T)[1],quantile(DW$spc[,spcmax],na.rm=T)[5])
      plot(1:nrow(DW$datrbind),DW$spc[,spcmax],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(DW$wl[spcmax],"nm"),axes=F,ylim=ylimp)
      xaxisdate(DW$datrbind$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(DW$spc[,spcmax2],na.rm=T)[1],quantile(DW$spc[,spcmax2],na.rm=T)[5])
      plot(1:nrow(DW$datrbind),DW$spc[,spcmax2],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(DW$wl[spcmax2],"nm"),axes=F,ylim=ylimp)
      xaxisdate(DW$datrbind$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(DW$spc[,spcmax3],na.rm=T)[1],quantile(DW$spc[,spcmax3],na.rm=T)[5])
      plot(1:nrow(DW$datrbind),DW$spc[,spcmax3],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(DW$wl[spcmax3],"nm"),axes=F,ylim=ylimp)
      xaxisdate(DW$datrbind$date, type = "n", xaxisdate_type)
      dev.off()
      message("DW plotted")
    }
  }
  # REF ####
  if("ref" %in% typeof){
    setwd(paste0(wd$work_horse,"CSV_SPC"))
    setwd("./ref")

    REF$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    REF$fileslist <- lapply(REF$fileslist,function(x) if(!identical(x, character(0))) x else{"///NA"})
    REF$filesdir <- lapply(REF$fileslist, function(x) if(length(grep("///NA",x))>0) "///NA" else{x})

    if(!is.na(product)){
      REF$fileslist <- REF$fileslist[match(substr(csvfiles$files,1,10),substr(unlist(REF$filesdir),1,10))]
      REF$filesdir <- REF$filesdir[match(substr(csvfiles$files,1,10),substr(unlist(REF$filesdir),1,10))]
    }

    REF$filesdir <- rmNullObs(REF$filesdir)
    REF$fileslist <- rmNullObs(REF$fileslist)

    REF$filesdir <- unique(unlist(REF$filesdir))

    REF$dat <-  suppressWarnings(lapply(REF$filesdir,function(x) read.csv2(x)))

    #zerotime
    zerotime <- which(do.call(rbind,lapply(REF$dat,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)

    REF$datetime <- lapply(REF$dat, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d_%H-%M-%S",tz="Europe/Berlin"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        REF$datetime[[zerotime[i]]] <- format(round(REF$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    REF$Produktnummer <- lapply(REF$dat,function(x) grep("Produkt",names(x)))
    REF$Produktnummer <- mapply(function(x , y) x[,y],REF$dat,REF$Produktnummer)
    REF$datrbind <- do.call(rbind.fill,REF$dat)
    names(REF$datrbind)[1:2] <- c("datetime","date")
    message(paste0("Done with reading reference spectra (.spc) files from ",wd$work_horse),"SPC/")

    names(REF$datrbind) <- gsub("X","",names(REF$datrbind))

    # Plot Ref ####
    if(fastplot==T){
      REF$datrbind$date <- as.POSIXct(REF$datrbind$date)

      if(length(unique(REF$datrbind$date))<=15) colp <- rainbow(length(unique(REF$datrbind$date)))
      if(length(unique(REF$datrbind$date))>15 & length(unique(REF$datrbind$date))<=30) colp <- rainbow(length(unique(week(REF$datrbind$date))))
      if(length(unique(REF$datrbind$date))>30) colp <- rainbow(length(unique(month(REF$datrbind$date))))

      if(length(unique(REF$datrbind$date))<=15) colp2 <- colp[factor(REF$datrbind$date)]
      if(length(unique(REF$datrbind$date))>15 & length(unique(REF$datrbind$date))<=30) colp2 <- colp[factor(week(REF$datrbind$date))]
      if(length(unique(REF$datrbind$date))>30) colp2 <- colp[factor(paste(substr(year(REF$datrbind$date), 3, 4), formatC(month(REF$datrbind$date), width = 2, flag = "0"), sep = "-"))]

      if(length(unique(REF$datrbind$date)) <= 15) legendtext <- as.character(unique(REF$datrbind$date))
      if(length(unique(REF$datrbind$date)) > 15 & length(unique(REF$datrbind$date))<=30) legendtext <- paste("KW",as.character(unique(week(REF$datrbind$date))))
      if(length(unique(REF$datrbind$date)) > 30) legendtext <- as.character(levels(factor(paste(substr(year(REF$datrbind$date), 3, 4), formatC(month(REF$datrbind$date), width = 2, flag = "0"), sep = "-"))))

      if(length(unique(REF$datrbind$date)) <= 15) xaxisdate_type <- "day"
      if(length(unique(REF$datrbind$date)) > 15 & length(unique(REF$datrbind$date))<=30) xaxisdate_type <- "cw"
      if(length(unique(REF$datrbind$date)) > 30) xaxisdate_type <- "year-month"

      png(filename=paste0(export_directory,"/",gsub("-","",gsub(".csv",".png",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",unit,"_",produktname,"_",product,"_ref.csv")))),width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(4,2.5,1.5,1))
      layout(matrix(c(1, 1, 2,
                      1, 1, 3,
                      4, 5, 6), nrow=3, byrow=TRUE))

      suppressWarnings(REF$dat <- REF$datrbind[, which(is.na(as.numeric(gsub("X","",names(REF$datrbind)))))])
      suppressWarnings(REF$spc <- REF$datrbind[, which(!is.na(as.numeric(gsub("X","",names(REF$datrbind)))))])
      suppressWarnings(REF$wl <- as.numeric(gsub("X","",names(REF$datrbind)[which(!is.na(as.numeric(gsub("X","",names(REF$datrbind)))))])))

      ylimp <- range(REF$spc,na.rm=T)
      xlimp <- range(REF$wl,na.rm=T)

      ifelse(!is.na(product),
             mainp <- paste0("Referenz vom ",firstday," bis ",lastday, "; ", as.character(input$product_ID$beverage[(input$product_ID$ID==product)]),"; ",location, " ", unit),
             mainp <- paste0("Referenz vom ",firstday," bis ",lastday, "; ",location, " ", unit))

      matplot(REF$wl,t(REF$spc),type="l", ylim=c(0,ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts",col=colp2,main=mainp)

      legend("topright",legendtext,cex=1,lty=1,col=colp,adj=0,ncol = ifelse(length(legendtext) < 21, 3, 2))

      spcmax <- which(REF$wl==215)
      spcmax2 <- which(REF$wl==266)
      spcmax3 <- which(REF$wl==485)

      plot(1:nrow(REF$datrbind),REF$datrbind$Integrationszeit,col=colp2,pch=19,cex=.4,type="p",xlab="",ylab="",main=paste("Integrationszeit"),axes=F)
      xaxisdate(REF$datrbind$date, type = "n", xaxisdate_type)

      plot(1:nrow(REF$datrbind),REF$datrbind$Mittelungen,col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste("Mittelungen"),axes=F)
      xaxisdate(REF$datrbind$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(REF$spc[,spcmax],na.rm=T)[2]*0.6,quantile(REF$spc[,spcmax],na.rm=T)[4]*1.4)
      plot(1:nrow(REF$datrbind),REF$spc[,spcmax],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(REF$wl[spcmax],"nm"),axes=F,ylim=ylimp)
      xaxisdate(REF$datrbind$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(REF$spc[,spcmax2],na.rm=T)[2]*0.6,quantile(REF$spc[,spcmax2],na.rm=T)[4]*1.4)
      plot(1:nrow(REF$datrbind),REF$spc[,spcmax2],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(REF$wl[spcmax2],"nm"),axes=F,ylim=ylimp)
      xaxisdate(REF$datrbind$date, type = "n", xaxisdate_type)

      ylimp <- c(quantile(REF$spc[,spcmax3],na.rm=T)[2]*0.6,quantile(REF$spc[,spcmax3],na.rm=T)[4]*1.4)
      plot(1:nrow(REF$datrbind),REF$spc[,spcmax3],col=colp2,pch=20,cex=.4,type="p",xlab="",ylab="",main=paste(REF$wl[spcmax3],"nm"),axes=F,ylim=ylimp)
      xaxisdate(REF$datrbind$date, type = "n", xaxisdate_type)
      dev.off()
      message("REF plotted")
    }
  }

  message(paste("Writing .csv files to",export_directory))

  # Export ####
  setwd(export_directory)

  if("spc" %in% typeof){suppressWarnings(SPC$merge[,which(names(SPC$merge) %in% names(Filter(is.factor, SPC$merge[1,-c(1:3)])))] <- apply(SPC$merge[,which(names(SPC$merge) %in% names(Filter(is.factor, SPC$merge[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}
  if("dw" %in% typeof){  suppressWarnings(DW$datrbind[,which(names(SPC$datrbind) %in% names(Filter(is.factor, DW$datrbind[1,-c(1:3)])))] <- apply(DW$datrbind[,which(names(DW$datrbind) %in% names(Filter(is.factor, DW$datrbind[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}
  if("ref" %in% typeof){suppressWarnings(REF$datrbind[,which(names(REF$datrbind) %in% names(Filter(is.factor, REF$datrbind[1,-c(1:3)])))] <- apply(REF$datrbind[,which(names(REF$datrbind) %in% names(Filter(is.factor, REF$datrbind[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}
  if("hand" %in% typeof) if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) {suppressWarnings(SPC$hand[,which(names(SPC$hand) %in% names(Filter(is.factor, SPC$hand[1,-c(1:3)])))] <- apply(SPC$hand[,which(names(SPC$hand) %in% names(Filter(is.factor, SPC$hand[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}

  if("spc" %in% typeof){suppressWarnings(SPC$merge[,which(names(SPC$merge) %in% names(Filter(is.factor, SPC$merge[1,-c(1:6)])))] <- apply(SPC$merge[,which(names(SPC$merge) %in% names(Filter(is.factor, SPC$merge[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}
  if("dw" %in% typeof){suppressWarnings(DW$datrbind[,which(names(SPC$datrbind) %in% names(Filter(is.factor, DW$datrbind[1,-c(1:6)])))] <- apply(DW$datrbind[,which(names(DW$datrbind) %in% names(Filter(is.factor, DW$datrbind[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}
  if("ref" %in% typeof){suppressWarnings(REF$datrbind[,which(names(REF$datrbind) %in% names(Filter(is.factor, REF$datrbind[1,-c(1:6)])))] <- apply(REF$datrbind[,which(names(REF$datrbind) %in% names(Filter(is.factor, REF$datrbind[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}
  if("hand" %in% typeof) if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) {suppressWarnings(SPC$hand[,which(names(SPC$hand) %in% names(Filter(is.factor, SPC$hand[1,-c(1:6)])))] <- apply(SPC$hand[,which(names(SPC$hand) %in% names(Filter(is.factor, SPC$hand[1,-c(1:3)])))],2,function(x) as.numeric(as.character(x))))}

  removecol <- c("Lambda", "Lamdba", "score")
  if("spc" %in% typeof) names(SPC$merge) <- gsub("ü", "ue", names(SPC$merge))
  if("spc" %in% typeof) names(SPC$merge) <- gsub("\\.x", "", names(SPC$merge))
  if("spc" %in% typeof) names(SPC$merge) <- gsub("\\.y", "", names(SPC$merge))

  if("drk" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(DW$datrbind)))) > 0) DW$datrbind <- DW$datrbind[ , -unique (grep(paste(removecol,collapse="|"), names(DW$datrbind)))]
  if("ref" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(REF$datrbind)))) > 0) REF$datrbind <- REF$datrbind[ , -unique (grep(paste(removecol,collapse="|"), names(REF$datrbind)))]
  if("spc" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(SPC$merge)))) > 0) SPC$merge <- SPC$merge[ , -unique (grep(paste(removecol,collapse="|"), names(SPC$merge)))]

  if("ref" %in% typeof) REF$datrbind[ , (max(grep("time", names(REF$datrbind))) + 1):ncol(REF$datrbind)] <- apply(REF$datrbind[ , (max(grep("time", names(REF$datrbind))) + 1):ncol(REF$datrbind)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
  if("drk" %in% typeof) DW$datrbind[ , (max(grep("time", names(DW$datrbind))) + 1):ncol(DW$datrbind)] <- apply(DW$datrbind[ , (max(grep("time", names(DW$datrbind))) + 1):ncol(DW$datrbind)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
  if("spc" %in% typeof) SPC$merge[ , (max(grep("Ringkessel", names(SPC$merge))) + 1):ncol(SPC$merge)] <- apply(SPC$merge[ , (max(grep("Ringkessel", names(SPC$merge))) + 1):ncol(SPC$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))

  if("spc" %in% typeof) write.csv2(SPC$merge,gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",unit,"_",produktname,"_",product,"_spc.csv")), row.names = F)
  if("drk" %in% typeof) write.csv2(DW$datrbind,gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",unit,"_",produktname,"_",product,"_drk.csv")), row.names = F)
  if("ref" %in% typeof) write.csv2(REF$datrbind,gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",unit,"_",produktname,"_",product,"_ref.csv")), row.names = F)
  if("hand" %in% typeof) if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) write.csv2(SPC$hand,gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",unit,"_",produktname,"_",product,"_hand.csv")), row.names = F)

  exportlist <- list()
  if("ref" %in% typeof) exportlist$ref <- REF$datrbind
  if("drk" %in% typeof) exportlist$drk <- DW$datrbind
  if("spc" %in% typeof) exportlist$spc <- SPC$merge
  if("hand" %in% typeof) if(length(SPC$hand) > 0 & !is.na(SPC$hand[1])) exportlist$spc <- SPC$hand

  message(paste("Export of .csv files to",export_directory, "finished"))
  return(exportlist)
  options(warn = oldw)
}



