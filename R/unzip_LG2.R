unzip.merge.LG2 <- function(year = year(Sys.Date())
                            , date = NA
                            , line = F
                            , just.list = F
                            , unzip.dir = "C:/csvtemp"
                            , customerlist = dt_customer){

  customerlist <- customerlist[ customerlist$LG != 3,]

  if(line==T){
    oo <- which(customerlist$customer == dt$customer &
                  customerlist$LG == dt$LG &
                  customerlist$location == dt$location &
                  customerlist$line == dt$line.choose)
  } else {oo <- 1:nrow(customerlist)
  }

  charactercol <- c("datetime", "date", "time", "Tank", "Auftrag", "Lfd..Ansatz", "Benutzer", "location", "unit", "DTproductName"
                    , "caffeineUNSB", "caffeineModel"
                    , "GS2UNSB", "GS2Model"
                    , "totalAcidUNSB", "totalAcidModel"
                    , "spectrumTimestamp", "statusTimestamp")

  for(o in oo){
    if(o == 9) next
    # set wd ####
    zip.dir <- paste0(service_backup_path(as.character(customerlist[o,]$customer), as.character(customerlist[o,]$location), as.character(customerlist[o,]$line)),"ZIP/")
    csv.dir <- paste0(service_backup_path(as.character(customerlist[o,]$customer), as.character(customerlist[o,]$location), as.character(customerlist[o,]$line)),"CSV/")
    ref.dir <- paste0(service_backup_path(as.character(customerlist[o,]$customer), as.character(customerlist[o,]$location), as.character(customerlist[o,]$line)), "ref/")
    drk.dir <- paste0(service_backup_path(as.character(customerlist[o,]$customer), as.character(customerlist[o,]$location), as.character(customerlist[o,]$line)), "drk/")
    spc.dir <- paste0(service_backup_path(as.character(customerlist[o,]$customer), as.character(customerlist[o,]$location), as.character(customerlist[o,]$line)), "spc/")

    # List zip.files ####
    setwd(zip.dir)
    zip.files <- dir(pattern = ".zip$")
    zip.files.date <- substr(zip.files,1,10)

    # List csv.files ####
    setwd(csv.dir)
    csv.files <- dir(pattern = ".csv$")
    csv.files.date <- substr(csv.files,1,10)

    # List exported csvfiles ####
    setwd(csv.dir)
    ref.files <- list.files(ref.dir, pattern = "ref.csv$")
    drk.files <- list.files(drk.dir, pattern = "drk.csv$")
    spc.files <- list.files(spc.dir, pattern = "spc.csv$")

    ref.drk.spc.files <- c(ref.files, drk.files, spc.files)
    ref.drk.spc.files.date <- substr(ref.drk.spc.files, 1, 10)

    # Filter year ####
    if(!is.na(year)){
      ref.drk.spc.files.date <- ref.drk.spc.files.date[grep(year, ref.drk.spc.files.date)]
      zip.files.date <- zip.files.date[grep(year,zip.files.date)]
    }

    # Filter date ####
    if(!is.na(date)){
      ref.drk.spc.files.date <- ref.drk.spc.files.date[grep(date,ref.drk.spc.files.date)]
      zip.files.date <- zip.files.date[grep(date,zip.files.date)]
    }

    zip.files.date <- sort(zip.files.date)
    ref.drk.spc.files.date <- sort(ref.drk.spc.files.date)

    # Stop when all files are already processed ####
    if(length(zip.files.date)==0){message(paste0("No new files found for ",
                                                 as.character(customerlist$customer[o])," ",
                                                 as.character(customerlist$location[o])," ",
                                                 as.character(customerlist$line[o]),", searched in ",
                                                 as.character(zip.dir)))
      setwd(csv.dir)
      next
    }

    # Check if files are already existing ####
    for(i in 1:length(sort(zip.files.date))){
      if(length(pmatch(zip.files.date[i] , sort(ref.drk.spc.files.date)))==1 &
         !is.na(pmatch(zip.files.date[i] , sort(ref.drk.spc.files.date)))){
        zip.files.date[i] <- NA}
    }

    zip.files.date <- zip.files.date[!is.na(zip.files.date)]

    # Stop when all files are already processed ####
    if(length(zip.files.date)==0){message(paste0("No new files found for ",
                                                 as.character(customerlist$customer[o])," ",
                                                 as.character(customerlist$location[o])," ",
                                                 as.character(customerlist$line[o]),", searched in ",
                                                 as.character(zip.dir)))
      setwd(csv.dir)
      next
    }

    # zip.files ####
    zip.files <- zip.files[zip.files %in% paste0(zip.files.date, ".zip")]

    # csv.files ####
    csv.files <- csv.files[substr(csv.files, 1, 10) %in% zip.files.date]

    # create unzip.dir ####
    setwd(unzip.dir)
    suppressWarnings(dir.create(unzip.dir))
    suppressWarnings(dir.create(paste(unzip.dir, "unzip", sep = "/")))
    suppressWarnings(dir.create(paste(unzip.dir, "unzip",as.character(customerlist[o,"customer"]), sep = "/")))
    suppressWarnings(dir.create(paste(unzip.dir, "unzip",as.character(customerlist[o,"customer"]),as.character(customerlist[o,"location"]), sep = "/")))
    suppressWarnings(dir.create(paste(unzip.dir, "unzip",as.character(customerlist[o,"customer"]),as.character(customerlist[o,"location"]),as.character(customerlist[o,"line"]), sep = "/")))

    suppressWarnings(dir.create(unzip.dir))
    suppressWarnings(dir.create(paste(unzip.dir, "zip", sep = "/")))
    suppressWarnings(dir.create(paste(unzip.dir, "zip",as.character(customerlist[o,"customer"]), sep = "/")))
    suppressWarnings(dir.create(paste(unzip.dir, "zip",as.character(customerlist[o,"customer"]),as.character(customerlist[o,"location"]), sep = "/")))
    suppressWarnings(dir.create(paste(unzip.dir, "zip",as.character(customerlist[o,"customer"]),as.character(customerlist[o,"location"]),as.character(customerlist[o,"line"]), sep = "/")))

    zip.dir.copy <- paste(unzip.dir, "zip",as.character(customerlist[o,"customer"]),as.character(customerlist[o,"location"]),as.character(customerlist[o,"line"]), sep = "/")
    unzip.dir.copy <- paste(unzip.dir, "unzip",as.character(customerlist[o,"customer"]),as.character(customerlist[o,"location"]),as.character(customerlist[o,"line"]), sep = "/")

    # Copy zip.files to zip.dir.copy ####
    setwd(zip.dir)
    file.copy(unlist(zip.files),paste(zip.dir.copy, unlist(zip.files),sep="/"),overwrite = T)

    # Create unz.log list with .log files ####
    if(customerlist[o,]$LG == "SG") unz.log <- lapply(unlist(zip.files),function(x) untar(x,list=T)) else{
      unz.log <- lapply(unlist(zip.files),function(x) untar(x,list=T))
      #unz.log <- lapply(unz.log,function(x) x$Name)
    }

    unz.log <- lapply(unz.log,function(x) grep(".log$",x,value=T))

    if(length(which(!is.na(unz.log)))==0){message(paste0("No new files found for ",
                                                         as.character(customerlist$customer[o])," ",
                                                         as.character(customerlist$location[o])," ",
                                                         as.character(customerlist$line[o]),", searched in ",
                                                         as.character(zip.dir)))
      setwd(csv.dir)
      next
    }

    unz.log <- lapply(unz.log,function(x) x[which(!is.na(x))])

    if(!is.list(unz.log)) {
      unz_log1 <- list()
      unz_log1[[1]] <- unz.log
      unz.log <- unz_log1
    }

    message(paste0(as.character(customerlist$customer[o])," ",
                   as.character(customerlist$location[o])," ",
                   as.character(customerlist$line[o])," ",
                   zip.files," to progress \n"))
    if(just.list==T) next

    # unzip ####
    for(j in 1:length(unz.log)){

      setwd(zip.dir.copy)

      # unzip
      if(customerlist[o,]$LG == "SG"){
        untar(unlist(zip.files)[j],unz.log[[j]],exdir=paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))} else {
          unzip(unlist(zip.files)[j],unz.log[[j]],exdir=paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))
        }

      # ref ####
      setwd(paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))

      ref <- list()
      ref$files.dir <- list.files(pattern="-R.log$", recursive = T)

      ref$dat <-  suppressWarnings(lapply(ref$files.dir,function(x) fread(x
                                                                          , header = F
                                                                          , sep = "="
                                                                          , dec = ","
                                                                          , encoding = "Latin-1")))

      ref$files.dir <- ref$files.dir[ which( unlist( lapply( ref$dat, length)) > 0 ) ]
      ref$dat <- ref$dat[ which( unlist( lapply( ref$dat, length)) > 0 ) ]

      ref$dat <- lapply(ref$dat,function(x) x[1:(which(x$V1=="Spektrum")-1),])

      zerotime <- which(do.call(rbind,lapply(ref$dat,function(x) ifelse(length(grep("00-00-00",x[1,2]))==0,0,1)))==1)
      ref$date <- lapply(ref$dat, function(x) as.POSIXct(as.character(x[1,2]),format="%Y-%m-%d_%H-%M-%S",tz="Europe/Berlin"))

      if(length(zerotime)>0){
        for(i in 1:length(zerotime))
          ref$date[[zerotime[i]]] <- format(round(ref$date[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")
      }

      ref$dat <- lapply(ref$dat,function(x) x[-1,])
      ref$para <- lapply(ref$dat,function(x) x$V1)

      ref$dat <-  lapply(ref$dat,function(x) x$V2 <- gsub(",",".",x$V2))

      ref$Produktnummer <- lapply(ref$para,function(x) grep("Produkt",x))
      ref$Produktnummer <- mapply(function(x , y) x[y],ref$dat,ref$Produktnummer)

      if(customerlist$LG[o]=="SG"){
        suppressWarnings(ref$spc <- lapply(unlist(ref$files.dir),function(x) fread(x,header=F,sep="=",dec=",",skip="Spektrum",fill=T)))

        ref$wl <- lapply(ref$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x$V2[1])),";"))))
        ref$spc <- lapply(ref$spc,function(x) as.numeric(unlist(strsplit(gsub(",",".",x$V1[2]),";"))))
      } else{
        suppressWarnings(ref$spc <- lapply(unlist(ref$files.dir),function(x) readLines(x)))

        ref$wl <- lapply(ref$spc, function(x) x[grep("Spektrum=", x)])
        ref$spc <- lapply(ref$spc, function(x) x[grep("Spektrum=", x)+1])

        ref$wl <- lapply(ref$wl, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x)),";"))))
        ref$spc <- lapply(ref$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",x),";"))))
      }

      if(length(ref$dat) > 0){
        if(customerlist$LG[o]!="SG"){ ref$dat <- lapply(ref$dat, function(x) data.table(t(as.numeric(as.character(gsub(",", ".", x))))))
        for(i in 1:length(ref$dat)) colnames(ref$dat[[i]]) <- ref$para[[i]]

        }

        if(customerlist$LG[o]=="SG"){
          sgnames <- c("datetime", "date", "time", "Tank", "Produkt", "Auftrag", "Lfd. Ansatz", "Benutzer")
          ref$dat1 <- mapply(function(dat, para) data.table(t(dat[ para %in% sgnames ]))
                             , dat = ref$dat
                             , para = ref$para
                             , SIMPLIFY = F)

          for(i in 1:length(ref$dat1)) colnames(ref$dat1[[i]]) <- ref$para[[i]][ref$para[[i]] %in% sgnames]

          ref$dat2 <- mapply(function(dat, para) data.table(t(as.numeric(as.character(gsub(",", ".", dat[ !para %in% sgnames ])))))
                             , dat = ref$dat
                             , para = ref$para
                             , SIMPLIFY = F)

          for(i in 1:length(ref$dat2)) colnames(ref$dat2[[i]]) <- ref$para[[i]][!ref$para[[i]] %in% sgnames]

          ref$dat <- mapply( function( x , y) data.table( x,y )
                  , x = ref$dat1
                  , y = ref$dat2
                  , SIMPLIFY = F)
          }

        ref$final <- data.table(datetime = do.call(rbind,lapply(ref$date,as.character))
                                , date = as.Date.character(do.call(rbind,lapply(ref$date,as.character)),tz="Europe/Berlin",format="%Y-%m-%d")
                                , time = substr(do.call(rbind,lapply(ref$date,as.character)),12,19)
                                , rbindlist(ref$dat, fill = T)
                                , do.call(rbind, ref$spc))

        colnames(ref$final)[1:3] <- c("datetime","date","time")
        rangep <- which(colnames(ref$final) == "Mittelungen") : ncol(ref$final)
        suppressWarnings(colnames(ref$final)[rangep][which(substr(colnames(ref$final)[rangep], 1, 1) == "V" & !is.na(as.numeric(substr(colnames(ref$final)[rangep], 2, 2))))] <- paste0("X", unlist(unique(ref$wl))))

        fwrite(ref$final
               , paste0(ref.dir,gsub(".zip","",unlist(zip.files)[j]),"_",as.character(customerlist[o,"customer"]),"_",as.character(customerlist[o,"location"]),"_",as.character(customerlist[o,"line"]),"_ref.csv")
               , sep = ";", dec = ",")
      }
      # drk ####
      setwd(paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))

      drk <- list()
      drk$files.dir <- list.files(pattern="-D.log$", recursive = T)

      drk$dat <-  suppressWarnings(lapply(drk$files.dir,function(x) fread(x
                                                                          , header = F
                                                                          , sep = "="
                                                                          , dec = ","
                                                                          , encoding = "Latin-1")))

      drk$files.dir <- drk$files.dir[ which( unlist( lapply( drk$dat, length)) > 0 ) ]
      drk$dat <- drk$dat[ which( unlist( lapply( drk$dat, length)) > 0 ) ]

      drk$dat <- lapply(drk$dat,function(x) x[1:(which(x$V1=="Spektrum")-1),])

      zerotime <- which(do.call(rbind,lapply(drk$dat,function(x) ifelse(length(grep("00-00-00",x[1,2]))==0,0,1)))==1)
      drk$date <- lapply(drk$dat, function(x) as.POSIXct(as.character(x[1,2]),format="%Y-%m-%d_%H-%M-%S",tz="Europe/Berlin"))

      if(length(zerotime)>0){
        for(i in 1:length(zerotime))
          drk$date[[zerotime[i]]] <- format(round(drk$date[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")
      }

      drk$dat <- lapply(drk$dat,function(x) x[-1,])
      drk$para <- lapply(drk$dat,function(x) x$V1)

      drk$dat <-  lapply(drk$dat,function(x) x$V2 <- gsub(",",".",x$V2))

      drk$Produktnummer <- lapply(drk$para,function(x) grep("Produkt",x))
      drk$Produktnummer <- mapply(function(x , y) x[y],drk$dat,drk$Produktnummer)

      if(customerlist$LG[o]=="SG"){
        suppressWarnings(drk$spc <- lapply(unlist(drk$files.dir),function(x) fread(x,header=F,sep="=",dec=",",skip="Spektrum",fill=T)))

        drk$wl <- lapply(drk$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x$V2[1])),";"))))
        drk$spc <- lapply(drk$spc,function(x) as.numeric(unlist(strsplit(gsub(",",".",x$V1[2]),";"))))
      } else{
        suppressWarnings(drk$spc <- lapply(unlist(drk$files.dir),function(x) readLines(x)))

        drk$wl <- lapply(drk$spc, function(x) x[grep("Spektrum=", x)])
        drk$spc <- lapply(drk$spc, function(x) x[grep("Spektrum=", x)+1])

        drk$wl <- lapply(drk$wl, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x)),";"))))
        drk$spc <- lapply(drk$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",x),";"))))
      }

      if(length(drk$dat) > 0){

        if(customerlist$LG[o]!="SG"){ drk$dat <- lapply(drk$dat, function(x) data.table(t(as.numeric(as.character(gsub(",", ".", x))))))
        for(i in 1:length(drk$dat)) colnames(drk$dat[[i]]) <- drk$para[[i]]
        }

        if(customerlist$LG[o]=="SG"){
          sgnames <- c("datetime", "date", "time", "Tank", "Produkt", "Auftrag", "Lfd. Ansatz", "Benutzer")
          drk$dat1 <- mapply(function(dat, para) data.table(t(dat[ para %in% sgnames ]))
                             , dat = drk$dat
                             , para = drk$para
                             , SIMPLIFY = F)

          for(i in 1:length(drk$dat1)) colnames(drk$dat1[[i]]) <- drk$para[[i]][drk$para[[i]] %in% sgnames]

          drk$dat2 <- mapply(function(dat, para) data.table(t(as.numeric(as.character(gsub(",", ".", dat[ !para %in% sgnames ])))))
                             , dat = drk$dat
                             , para = drk$para
                             , SIMPLIFY = F)

          for(i in 1:length(drk$dat2)) colnames(drk$dat2[[i]]) <- drk$para[[i]][!drk$para[[i]] %in% sgnames]

          drk$dat <- mapply( function( x , y) data.table( x,y )
                             , x = drk$dat1
                             , y = drk$dat2
                             , SIMPLIFY = F)
        }

        drk$final <- data.table(datetime = do.call(rbind,lapply(drk$date,as.character))
                                , date = as.Date.character(do.call(rbind,lapply(drk$date,as.character)),tz="Europe/Berlin",format="%Y-%m-%d")
                                , time = substr(do.call(rbind,lapply(drk$date,as.character)),12,19)
                                , rbindlist(drk$dat, fill = T)
                                , do.call(rbind, drk$spc))

        colnames(drk$final)[1:3] <- c("datetime","date","time")
        rangep <- which(colnames(drk$final) == "Mittelungen") : ncol(drk$final)
        suppressWarnings(colnames(drk$final)[rangep][which(substr(colnames(drk$final)[rangep], 1, 1) == "V" & !is.na(as.numeric(substr(colnames(drk$final)[rangep], 2, 2))))] <- paste0("X", unlist(unique(drk$wl))))

        fwrite(drk$final
               , paste0(drk.dir,gsub(".zip","",unlist(zip.files)[j]),"_",as.character(customerlist[o,"customer"]),"_",as.character(customerlist[o,"location"]),"_",as.character(customerlist[o,"line"]),"_drk.csv")
               , sep = ";", dec = ",")
      }
      # spc ####
      setwd(paste(unzip.dir.copy,gsub(".zip","",unlist(zip.files)[j]),sep="/"))

      spc <- list()
      spc$files.dir <- list.files(pattern="-M.log$", recursive = T)

      spc$dat <-  suppressWarnings(lapply(spc$files.dir,function(x) fread(x
                                                                          , header = F
                                                                          , sep = "="
                                                                          , dec = ","
                                                                          , encoding = "Latin-1")))

      spc$files.dir <- spc$files.dir[ which( unlist( lapply( spc$dat, length)) > 0 ) ]
      spc$dat <- spc$dat[ which( unlist( lapply( spc$dat, length)) > 0 ) ]

      spc$dat <- lapply(spc$dat,function(x) x[1:(which(x$V1=="Spektrum")-1),])

      zerotime <- which(do.call(rbind,lapply(spc$dat,function(x) ifelse(length(grep("00-00-00",x[1,2]))==0,0,1)))==1)
      spc$date <- lapply(spc$dat, function(x) as.POSIXct(as.character(x[1,2]),format="%Y-%m-%d_%H-%M-%S",tz="Europe/Berlin"))

      if(length(zerotime)>0){
        for(i in 1:length(zerotime))
          spc$date[[zerotime[i]]] <- format(round(spc$date[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")
      }

      spc$dat <- lapply(spc$dat,function(x) x[-1,])
      spc$para <- lapply(spc$dat,function(x) x$V1)

      spc$dat <-  lapply(spc$dat,function(x) x$V2 <- gsub(",",".",x$V2))

      spc$Produktnummer <- lapply(spc$para,function(x) grep("Produkt",x))
      spc$Produktnummer <- mapply(function(x , y) x[y],spc$dat,spc$Produktnummer)

      if(customerlist$LG[o]=="SG"){
        suppressWarnings(spc$spc <- lapply(unlist(spc$files.dir),function(x) fread(x,header=F,sep="=",dec=",",skip="Spektrum",fill=T)))

        spc$wl <- lapply(spc$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x$V2[1])),";"))))
        spc$spc <- lapply(spc$spc,function(x) as.numeric(unlist(strsplit(gsub(",",".",x$V1[2]),";"))))
      } else{
        suppressWarnings(spc$spc <- lapply(unlist(spc$files.dir),function(x) readLines(x)))

        spc$wl <- lapply(spc$spc, function(x) x[grep("Spektrum=", x)])
        spc$spc <- lapply(spc$spc, function(x) x[grep("Spektrum=", x)+1])

        spc$wl <- lapply(spc$wl, function(x) as.numeric(unlist(strsplit(gsub(",",".",gsub("Spektrum=","",x)),";"))))
        spc$spc <- lapply(spc$spc, function(x) as.numeric(unlist(strsplit(gsub(",",".",x),";"))))
      }

      if(length(spc$dat) > 0){
        if(customerlist$LG[o]!="SG"){ spc$dat <- lapply(spc$dat, function(x) data.table(t(as.numeric(as.character(gsub(",", ".", x))))))
        for(i in 1:length(spc$dat)) colnames(spc$dat[[i]]) <- spc$para[[i]]
        }

        if(customerlist$LG[o]=="SG"){
          sgnames <- c("datetime", "date", "time", "Tank", "Produkt", "Auftrag", "Lfd. Ansatz", "Benutzer")
          spc$dat1 <- mapply(function(dat, para) data.table(t(dat[ para %in% sgnames ]))
                             , dat = spc$dat
                             , para = spc$para
                             , SIMPLIFY = F)

          for(i in 1:length(spc$dat1)) colnames(spc$dat1[[i]]) <- spc$para[[i]][spc$para[[i]] %in% sgnames]

          spc$dat2 <- mapply(function(dat, para) data.table(t(as.numeric(as.character(gsub(",", ".", dat[ !para %in% sgnames ])))))
                             , dat = spc$dat
                             , para = spc$para
                             , SIMPLIFY = F)
          for(i in 1:length(spc$dat2)) colnames(spc$dat2[[i]]) <- spc$para[[i]][!spc$para[[i]] %in% sgnames]

          spc$dat <- mapply( function( x , y) data.table( x,y )
                             , x = spc$dat1
                             , y = spc$dat2
                             , SIMPLIFY = F)
        }

        spc$final <- data.table(datetime = do.call(rbind,lapply(spc$date,as.character))
                                , date = as.Date.character(do.call(rbind,lapply(spc$date,as.character)),tz="Europe/Berlin",format="%Y-%m-%d")
                                , time = substr(do.call(rbind,lapply(spc$date,as.character)),12,19)
                                , rbindlist(spc$dat, fill = T)
                                , do.call(rbind, spc$spc))

        colnames(spc$final)[1:3] <- c("datetime","date","time")
        rangep <- which(colnames(spc$final) == "Mittelungen") : ncol(spc$final)
        suppressWarnings(colnames(spc$final)[rangep][which(substr(colnames(spc$final)[rangep], 1, 1) == "V" & !is.na(as.numeric(substr(colnames(spc$final)[rangep], 2, 2))))] <- paste0("X", unlist(unique(spc$wl))))

        fwrite(spc$final
               , paste0(spc.dir,gsub(".zip","",unlist(zip.files)[j]),"_",as.character(customerlist[o,"customer"]),"_",as.character(customerlist[o,"location"]),"_",as.character(customerlist[o,"line"]),"_spc.csv")
               , sep = ";", dec = ",")
      }
      message(paste(gsub(".zip","",unlist(zip.files)[j]),"for",as.character(customerlist$customer[o]),as.character(customerlist$location[o]),as.character(customerlist$line[o]),"completed, time =",Sys.time(),length(unz.log)-j,"to go"))

    }

    setwd(csv.dir)
    unallowed <- c("I:","K:","L:","M:","N:","P:","R:","S:","W:")
    if(length(unique(match(unallowed,substr(zip.dir.copy,1,2))))<2){
      unlink(paste(zip.dir.copy,"*",sep="/"),recursive = T,force=T)
      unlink(paste(unzip.dir.copy,"*",sep="/"),recursive = T,force=T)
    } # else {textreturn <- "No files are removed as you try to automatically remove files from the server"}

    message(paste(as.character(customerlist$customer[o]),as.character(customerlist$location[o]),as.character(customerlist$line[o]),"finished"))

    message("Create production list per day")

    for(i in substr(unique(unlist(zip.files)), 1, 10)){
      produkt_per_day_year(customer = customerlist$customer[o]
                           , location = customerlist$location[o]
                           , line = customerlist$line[o]
                           , LG = "2"
                           , year = year
                           , date = i)
    }
    message("finished")
  }


}
