# SG read and merge csv files ####
read.csv.SG <- function(customer,location,line,firstday,lastday,product=NA,export_directory="C://csvtemp",
                               typeof = c("ref","drk","spc"), fastplot){

  oldw <- getOption("warn")
  options(warn = -1)

  # create export dir, emtpy lists and setwd ####
  dir.create(export_directory,showWarnings = F)

  if("spc" %in% typeof) SPC <- list()
  if("drk" %in% typeof) DW <- list()
  if("ref" %in% typeof) REF <- list()

  if("spc" %in% typeof){
    # SPC ####

    setwd(service_backup_path(customer, location, line))
    setwd("./spc")

    SPC$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    SPC$fileslist <- lapply(SPC$fileslist,function(x) if(!identical(x, character(0))) x else{"///NA"})
    SPC$filesdir <- lapply(SPC$fileslist, function(x) if(length(grep("///NA",x))>0) "///NA" else{x})

    SPC$filesdir <- rmNullObs(SPC$filesdir)
    SPC$fileslist <- rmNullObs(SPC$fileslist)

    SPC$dat <-  suppressWarnings(lapply(SPC$filesdir,function(x) fread(x, sep = ";", dec = ",")))

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

    for(i in length(SPC$dat):1) if(nrow(SPC$dat[[i]])==0) SPC$dat[[i]] <- NULL

    SPC$dat <- lapply(SPC$dat,function(x) x <- x[, colSums(is.na(x)) != nrow(x), with = F])

    # zerotime
    zerotime <- which(do.call(rbind,lapply(SPC$dat,function(x) ifelse(length(grep("00-00-00",x[1,"time",with = F]))==0,0,1)))==1)

    SPC$datetime <- lapply(SPC$dat, function(x) as.POSIXct(as.character(x[1,"datetime",with = F]),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        SPC$datetime[[zerotime[i]]] <- format(round(SPC$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    SPC$Produktnummer <- lapply(SPC$dat,function(x) grep("Produkt",names(x)))
    SPC$Produktnummer <- mapply(function(x , y) x[,y,with=F],SPC$dat,SPC$Produktnummer)


      for(i in 1:length(SPC$dat)){
        istext <- which(names(SPC$dat[[i]]) %in% c("datetime","date","time","Tank","Produkt","Auftrag","Benutzer"))
        istextl <- ifelse(length(istext)>0,length(istext),0)
        isfac <- which(sapply(SPC$dat[[i]][,-c(istext)], is.factor))+istextl
        if(length(isfac)>0) for(j in as.numeric(isfac)) SPC$dat[[i]][,j] <- as.numeric(as.character(SPC$dat[[i]][,j]))
      }
    SPC$datrbind <- rbindlist(SPC$dat, fill = T)

    suppressWarnings(movevec <- names(SPC$datrbind)[which(!is.na(as.numeric(gsub("X","",names(SPC$datrbind)))))])
    for(i in 1:length(movevec)){
      col_idx <- grep(movevec[i],names(SPC$datrbind))
      SPC$datrbind <- SPC$datrbind[, c((1:ncol(SPC$datrbind))[-col_idx],col_idx), with = F]}

    if(!is.na(product))  SPC$datrbind <- SPC$datrbind[ SPC$datrbind$Produkt %in% product , ]
    if(!is.na(product) & nrow(SPC$datrbind) == 0) stop(paste0("No production of ", product, " in the chosen timeframe!"))

    SPC$merge <- SPC$datrbind

    SPC$merge <- SPC$merge[which(!duplicated(SPC$merge$datetime)),]

    if(length(grep("Produktnummer",names(SPC$merge)))>1) SPC$merge <- SPC$merge[,-grep("Produktnummer",names(SPC$merge))[2], with = F]
    if(length(grep("Scores",names(SPC$merge)))>0) SPC$merge <- SPC$merge[,-grep("Scores",names(SPC$merge)), with = F]
    if(length(grep("Produktprofil",names(SPC$merge)))>0) SPC$merge <- SPC$merge[,-grep("Produktprofil",names(SPC$merge)), with = F]
    if(length(grep("score",names(SPC$merge)))>0) SPC$merge <- SPC$merge[,-grep("score",names(SPC$merge)), with = F]
    if(length(which(unlist(gregexpr("Lambda",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("Lambda",names(SPC$merge)))>0)), with = F]
    if(length(which(unlist(gregexpr("Lamdba",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("Lamdba",names(SPC$merge)))>0)), with = F]
    if(length(which(unlist(gregexpr("leverages",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("leverages",names(SPC$merge)))>0)), with = F]
    if(length(which(unlist(gregexpr("XValSamp",names(SPC$merge)))>0))>0) SPC$merge <- SPC$merge[,-c(which(unlist(gregexpr("XValSamp",names(SPC$merge)))>0)), with = F]

    names(SPC$merge)[1:2] <- c("datetime","date")

    message(paste0("Done with reading product spectra (.spc) files from ",wd$work_horse),"SPC/")

    names(SPC$merge) <- gsub("X","",names(SPC$merge))
   }

  if("drk" %in% typeof){
    # DW ####
    setwd(service_backup_path(customer, location, line))
    setwd("./drk")

    DW$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    DW$fileslist <- lapply(DW$fileslist,function(x) if(!identical(x, character(0))) x else{"///NA"})
    DW$filesdir <- lapply(DW$fileslist, function(x) if(length(grep("///NA",x))>0) "///NA" else{x})

    DW$filesdir <- rmNullObs(DW$filesdir)
    DW$fileslist <- rmNullObs(DW$fileslist)

    DW$dat <-  suppressWarnings(lapply(DW$filesdir,function(x) read.csv2(x)))

    # zerotime
    zerotime <- which(do.call(rbind,lapply(DW$dat,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
    DW$datetime <- lapply(DW$dat, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin"))

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

  }
  # REF ####
  if("ref" %in% typeof){
    setwd(service_backup_path(customer, location, line))
    setwd("./ref")

    REF$fileslist <- dir(pattern=".csv")[which(substr(dir(pattern=".csv"),1,10)>=firstday & substr(dir(pattern=".csv"),1,10)<=lastday)]
    REF$fileslist <- lapply(REF$fileslist,function(x) if(!identical(x, character(0))) x else{"///NA"})
    REF$filesdir <- lapply(REF$fileslist, function(x) if(length(grep("///NA",x))>0) "///NA" else{x})

    REF$filesdir <- rmNullObs(REF$filesdir)
    REF$fileslist <- rmNullObs(REF$fileslist)

    REF$dat <-  suppressWarnings(lapply(REF$filesdir,function(x) read.csv2(x)))

    #zerotime
    zerotime <- which(do.call(rbind,lapply(REF$dat,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)

    REF$datetime <- lapply(REF$dat, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d %H:%M:%S",tz="Europe/Berlin"))

    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        REF$datetime[[zerotime[i]]] <- format(round(REF$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    REF$Produktnummer <- lapply(REF$dat,function(x) grep("Produkt",names(x)))
    REF$Produktnummer <- mapply(function(x , y) x[,y],REF$dat,REF$Produktnummer)
    REF$datrbind <- do.call(rbind.fill,REF$dat)
    names(REF$datrbind)[1:2] <- c("datetime","date")
    message(paste0("Done with reading reference spectra (.spc) files from ",wd$work_horse),"SPC/")

    names(REF$datrbind) <- gsub("X","",names(REF$datrbind))


  }

  message(paste("Writing .csv files to",export_directory))

  if(exists("SPC")) if(length(which(names(SPC$merge) == "Benutzer")) > 0){
    if("spc" %in% typeof){
      SPC$merge <- data.frame(SPC$merge)
    SPC$merge[ , (which(names(SPC$merge) == "Benutzer") + 1) : ncol(SPC$merge)] <- apply(SPC$merge[ , (which(names(SPC$merge) == "Benutzer") + 1) : ncol(SPC$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
    }
  }
  if(exists("DW")) if(length(which(names(DW$merge) == "Benutzer")) > 0){
    if("drk" %in% typeof) DW$merge[ , (which(names(DW$merge) == "Benutzer") + 1) : ncol(DW$merge)] <- apply(DW$merge[ , (which(names(DW$merge) == "Benutzer") + 1) : ncol(DW$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
  }
  if(exists("REF")) if(length(which(names(REF$merge) == "Benutzer")) > 0){
    if("ref" %in% typeof) REF$merge[ , (which(names(REF$merge) == "Benutzer") + 1) : ncol(REF$merge)] <- apply(REF$merge[ , (which(names(REF$merge) == "Benutzer") + 1) : ncol(REF$merge)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
  }

  if(exists("SPC")) if(length(which(names(SPC$merge) == "Benutzer")) == 0){
    if("spc" %in% typeof){
      nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(SPC$merge, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
      SPC$merge[, nums] <- apply(SPC$merge[,nums],2,function(x) as.numeric(as.character(x)))

    }
  }
  if(exists("DW")) if(length(which(names(DW$merge) == "Benutzer")) > 0){
    if("drk" %in% typeof){
      nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(DW$datrbind, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
      DW$datrbind[, nums] <- apply(DW$datrbind[,nums],2,function(x) as.numeric(as.character(x)))
    }
  }
  if(exists("REF")) if(length(which(names(REF$merge) == "Benutzer")) > 0){
    if("ref" %in% typeof){
      nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(REF$datrbind, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
      REF$datrbind[, nums] <- apply(REF$datrbind[,nums],2,function(x) as.numeric(as.character(x)))
    }
  }

  # Export ####
  setwd(export_directory)

  if("spc" %in% typeof){
    SPC$merge[ , grep("Benutzer",names(SPC$merge))] <- NULL
    for(i in 7:ncol(SPC$merge))   SPC$merge[,i] <- as.numeric(as.character(SPC$merge[,i]))
  }

  removecol <- c("Lambda", "Lamdba", "score")
  if("spc" %in% typeof) names(SPC$merge) <- gsub("ü", "ue", names(SPC$merge))
  if("spc" %in% typeof) names(SPC$merge) <- gsub("\\.x", "", names(SPC$merge))
  if("spc" %in% typeof) names(SPC$merge) <- gsub("\\.y", "", names(SPC$merge))

  if("drk" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(DW$datrbind)))) > 0) DW$datrbind <- DW$datrbind[ , -unique (grep(paste(removecol,collapse="|"), names(DW$datrbind)))]
  if("ref" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(REF$datrbind)))) > 0) REF$datrbind <- REF$datrbind[ , -unique (grep(paste(removecol,collapse="|"), names(REF$datrbind)))]
  if("spc" %in% typeof) if(length(unique (grep(paste(removecol,collapse="|"), names(SPC$merge)))) > 0) SPC$merge <- SPC$merge[ , -unique (grep(paste(removecol,collapse="|"), names(SPC$merge)))]

  if("ref" %in% typeof) REF$datrbind[ , (max(grep("time", names(REF$datrbind))) + 1):ncol(REF$datrbind)] <- apply(REF$datrbind[ , (max(grep("time", names(REF$datrbind))) + 1):ncol(REF$datrbind)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))
  if("drk" %in% typeof) DW$datrbind[ , (max(grep("time", names(DW$datrbind))) + 1):ncol(DW$datrbind)] <- apply(DW$datrbind[ , (max(grep("time", names(DW$datrbind))) + 1):ncol(DW$datrbind)], 2, function(x) as.numeric(as.character(gsub(",",".",x))))

  if("spc" %in% typeof) write.csv2(SPC$merge,gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",line,"_",gsub("%","p",dt$product),"_spc.csv")), row.names = F)
  if("drk" %in% typeof) write.csv2(DW$datrbind,gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",line,"_",gsub("%","p",dt$product),"_drk.csv")), row.names = F)
  if("ref" %in% typeof) write.csv2(REF$datrbind,gsub("-","",paste0(substr(firstday,3,nchar(firstday)),"_",substr(lastday,3,nchar(lastday)),"_",location,"_",line,"_",gsub("%","p",dt$product),"_ref.csv")), row.names = F)

  exportlist <- list()
  if("ref" %in% typeof) exportlist$ref <- REF$datrbind
  if("drk" %in% typeof) exportlist$drk <- DW$datrbind
  if("spc" %in% typeof) exportlist$spc <- SPC$merge

  message(paste("Export of .csv files to",export_directory, "finished"))
  return(exportlist)
  options(warn = oldw)
}



