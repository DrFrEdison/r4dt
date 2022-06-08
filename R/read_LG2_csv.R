# LG2 read and merge csv files ####
read.csv.LG2 <- function(customer
                         , location
                         , line
                         , firstday
                         , lastday
                         , product= NA
                         , export_directory="C://csvtemp"
                         , Ringkessel = T
                         , typeof = c("ref","drk","spc")
                         , slim = T
                         , return.R = F
                         , product_ID = dt$product_ID){

  if(slim == T) coltoremove <- c("score", "Lamdba.Start", "Lambda.Ende", "Lambda.Delta", "leverages", "XValSamp", "Scores", "Produktprofil", "stopp", "Ring")

  # create export dir, emtpy lists and setwd ####
  if(!is.na(export_directory)) dir.create(export_directory,showWarnings = F)

  read <- list()

  read$wd.o <- getwd()
  read$wd <- .service_backup_path(customer, location, line)
  setwd(read$wd)

  # name of export files
  read$name$date1 <- gsub("-","",substr(as.character(firstday),3,nchar(as.character(firstday))))
  read$name$date2 <- gsub("-","",substr(as.character(lastday),3,nchar(as.character(lastday))))

  product_ID <- data.frame(product_ID)
  product_ID <- product_ID[product_ID$customer == customer,]
  product_ID <- product_ID[product_ID$location == location,]
  if(nchar(unique(product_ID$line)[1]) != 0)   product_ID <- product_ID[product_ID$line == line,]
  read$name$product <- unique(product_ID$beverage[which(product_ID$ID==product)])

  read$name$file <- paste(read$name$date1, read$name$date2, location, line, read$name$product, paste(product, collapse = "_"), sep = "_")
  read$name$file <- gsub("__", "_", read$name$file)
  if(is.na(product)) read$name$file <- paste(read$name$date1, read$name$date2, location, line, sep = "_")
  # csv files ####
  csvfiles <- list()
  setwd("./CSV")
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

  if(length(csvfiles$files)==0) stop(paste0("No production of ", read$name$product," (",product,") in the chosen timeframe!"))

  csvfiles$dat <- lapply(csvfiles$files,function(x) fread(x,sep="\t",dec=",",header=F,encoding = "UTF-8",skip=1))
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

  csvfiles$rbind$Zeitstempel <- as.character(csvfiles$rbind$Zeitstempel)
  message(paste0("Done with reading .csv files from ",wd$work_horse),"CSV/")

  # Reference Spectra ####
  if(any(grepl( "ref", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./ref")
    ref <- list()

    # list ref files
    ref$files <- dir(pattern = "_ref.csv")[which(substr(dir(pattern = "_ref.csv"), 1, 10)>=firstday & substr(dir(pattern = "_ref.csv"), 1, 10)<=lastday)]

    # match csv and ref files by date
    if(!is.na(product)){ref$files <- ref$files[match(substr(csvfiles$files,1,10),substr(unlist(ref$files),1,10))]}
    ref$files <- sort(ref$files )

    # read ref files
    ref$merge <- lapply(ref$files,function(x) fread(x, dec = ",", sep = ";"))

    #zerotime
    zerotime <- which(do.call(rbind,lapply(ref$merge,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
    ref$datetime <- lapply(ref$merge, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d_%H-%M-%S",tz="UTC"))
    if(length(zerotime)>0){
      for(i in 1:length(zerotime))
        ref$datetime[[zerotime[i]]] <- format(round(ref$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

    # merge
    ref$merge <- rbindlist(ref$merge, fill = T)

    # datetime
    ref$merge$date <- as.Date(as.POSIXct(as.character(ref$merge$date),format="%Y-%m-%d",tz="UTC"))
    ref$merge$datetime <- as.POSIXct(as.character(ref$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

    # order columns
    if(is.na(as.numeric(gsub("X", "", names(ref$merge)[ncol(ref$merge)])))){
      suppressWarnings(ref$wl <- sort(as.numeric(gsub("X","",names(ref$merge)))))
      for(i in paste0("X", ref$wl)) ref$merge <- ref$merge[ , moveme(names(ref$merge), paste(i, "last")), with = F]}

    # slim data
    if(slim == T) ref$merge <- ref$merge[ , names(ref$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(ref$merge)))))], with = F]
    if(slim == T) ref$merge$date <- NULL
    if(slim == T) ref$merge$time <- NULL

    if(!is.na(export_directory)){
    # write
    setwd(export_directory)
    fwrite(x = ref$merge, file = paste0(read$name$file,"_ref.csv"), sep = ";", dec = ",")
    message("Reference Spectra exported")}
  }

  # Dark Spectra ####
  if(any(grepl( "drk", typeof, fixed = TRUE))){

      setwd(read$wd)
      setwd("./drk")
      drk <- list()

      # list drk files
      drk$files <- dir(pattern = "_drk.csv")[which(substr(dir(pattern = "_drk.csv"), 1, 10)>=firstday & substr(dir(pattern = "_drk.csv"), 1, 10)<=lastday)]

      # match csv and drk files by date
      if(!is.na(product)){drk$files <- drk$files[match(substr(csvfiles$files,1,10),substr(unlist(drk$files),1,10))]}
      drk$files <- sort(drk$files )
      # read drk files
      drk$merge <- lapply(drk$files,function(x) fread(x, dec = ",", sep = ";"))

      #zerotime
      zerotime <- which(do.call(rbind,lapply(drk$merge,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
      drk$datetime <- lapply(drk$merge, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d_%H-%M-%S",tz="UTC"))
      if(length(zerotime)>0){
        for(i in 1:length(zerotime))
          drk$datetime[[zerotime[i]]] <- format(round(drk$datetime[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

      # merge
      drk$merge <- rbindlist(drk$merge, fill = T)

      # datetime
      drk$merge$date <- as.Date(as.POSIXct(as.character(drk$merge$date),format="%Y-%m-%d",tz="UTC"))
      drk$merge$datetime <- as.POSIXct(as.character(drk$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

      # order columns
      if(is.na(as.numeric(gsub("X", "", names(drk$merge)[ncol(drk$merge)])))){
        suppressWarnings(drk$wl <- sort(as.numeric(gsub("X","",names(drk$merge)))))
        for(i in paste0("X", drk$wl)) drk$merge <- drk$merge[ , moveme(names(drk$merge), paste(i, "last")), with = F]}

      # slim data
      if(slim == T) drk$merge <- drk$merge[ , names(drk$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(drk$merge)))))], with = F]
      if(slim == T) drk$merge$date <- NULL
      if(slim == T) drk$merge$time <- NULL

      if(!is.na(export_directory)){
      # write
      setwd(export_directory)
      fwrite(x = drk$merge, file = paste0(read$name$file,"_drk.csv"), sep = ";", dec = ",")
      message("Dark Spectra exported")}
    }

  # spc ####
  if(any(grepl( "spc", typeof, fixed = TRUE))){
      gc()
      setwd(read$wd)
      setwd("./spc")

      spc <- list()

      # list spc files
      spc$files <- dir(pattern = "_spc.csv")[which(substr(dir(pattern = "_spc.csv"), 1, 10)>=firstday & substr(dir(pattern = "_spc.csv"), 1, 10)<=lastday)]

      if(!is.na(product)){spc$files <- spc$files[match(substr(csvfiles$files,1,10),substr(unlist(spc$files),1,10))]}
      spc$files <- sort(spc$files)

      spc$merge <-  suppressWarnings(lapply(spc$files,function(x) fread(x, dec = ",", sep = ";")))

      for(i in 1:length(spc$merge)){
        names(spc$merge[[i]])[grep("Produktn", names(spc$merge[[i]]))] <- "Produktnummer"
      }

      # German Acid name Problem äöüß
      for(i in 1:length(spc$merge)){
        find_acid <- grep("ure.",names(spc$merge[[i]]))
        names_acid <- names(spc$merge[[i]])[find_acid]

        if( sum(unlist(lapply(gregexpr("\\.", names_acid), min))) < 0 ){
          names_acid <- paste0("Sae", substr(names_acid, 3, nchar(names_acid)))
          names(spc$merge[[i]])[find_acid] <- names_acid
          }

        if( sum(unlist(lapply(gregexpr("\\.", names_acid), min))) > 0 ){
          names_acid1 <- substr(names_acid, 1, unlist(lapply(gregexpr("\\.", names_acid), min))-1)
          names_acid2 <- substr(names_acid, unlist(lapply(gregexpr("_", names_acid), max)), nchar(names_acid))

        names(spc$merge[[i]])[find_acid] <- gsub("SaeureSaeure", "Saeure", gsub("\\.","",paste(names_acid1, names_acid2, sep = ".")))}
      }

      # filter by product
      if(!is.na(product)) spc$merge <- lapply(spc$merge, function(x) x[x$Produktnummer %in% product , ])

      # zerotime
      zerotime <- which(do.call(rbind,lapply(spc$merge,function(x) ifelse(length(grep("00-00-00",x[1,"time"]))==0,0,1)))==1)
      spc$merge.time <- lapply(spc$merge, function(x) as.POSIXct(as.character(x[1,"datetime"]),format="%Y-%m-%d %H:%M:%S",tz="UTC"))
      if(length(zerotime)>0){
        for(i in 1:length(zerotime))
          spc$merge.time[[zerotime[i]]] <- format(round(spc$merge.time[[zerotime[i]]],units="days"),"%Y-%m-%d %M:%H:%S")}

      # merge
      spc$merge <- rbindlist(spc$merge, fill = T)

      if(slim == F) spc$merge$date <- as.Date(as.POSIXct(as.character(spc$merge$date),format="%Y-%m-%d",tz="UTC"))
      spc$merge$datetime <- as.character(spc$merge$datetime)

      # move wl to last columns if necessary
      if(is.na(as.numeric(gsub("X", "", names(spc$merge)[ncol(spc$merge)])))){
        suppressWarnings(spc$wl <- sort(as.numeric(gsub("X","",names(spc$merge)))))
        for(i in paste0("X", spc$wl)) spc$merge <- spc$merge[ , moveme(names(spc$merge), paste(i, "last")), with = F]}

    if(length(which(!(spc$merge$datetime %in% csvfiles$rbind$Zeitstempel) == T))>0)  spc$merge <- spc$merge[ - which(!(spc$merge$datetime %in% csvfiles$rbind$Zeitstempel) == T),]
    spc$merge <- merge.data.frame(csvfiles$rbind,spc$merge,by.x="Zeitstempel",by.y="datetime")
    spc$merge <- spc$merge[colSums(!is.na(spc$merge)) > 0]

    # filter by typecode
    if(Ringkessel) spc$merge <- spc$merge[spc$merge$Ringkessel ==  Ringkessel , ]

    if(slim == F) spc$merge <- spc$merge[, moveme(names(spc$merge), "Zeitstempel date time first")]
    if(slim == T) spc$merge <- spc$merge[, moveme(names(spc$merge), "Zeitstempel first")]
    colnames(spc$merge)[1] <- "datetime"
    spc$merge$datetime <- as.POSIXct(as.character(spc$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

    spc$merge <- spc$merge[order(spc$merge$datetime),]

    spc$merge <- spc$merge[which(!duplicated(spc$merge$datetime)),]
    spc$merge <- spc$merge[ , which(!names(spc$merge) == "Produktnummer.y")]

    names(spc$merge) <- gsub(".x", "", names(spc$merge))

    if(slim == T) spc$merge <- spc$merge[ , - unlist(lapply(coltoremove, function(x) grep(x, names(spc$merge))))]

    if(slim == F) spc$merge <- spc$merge[, moveme(names(spc$merge), "datetime date time first")]
    if(slim == T) spc$merge <- spc$merge[, moveme(names(spc$merge), "datetime first")]
    if(slim == T) spc$merge$date <- NULL
    if(slim == T) spc$merge$time <- NULL

    if(!is.na(export_directory)){
    # write
    setwd(export_directory)
    fwrite(x = spc$merge, file = paste0(read$name$file,"_spc.csv"), sep = ";", dec = ",")
    message("Production Spectra exported")}
  }

  if(!is.na(export_directory))  message(paste("Export of .csv files to",export_directory, "finished"))
  gc()
  if(!is.na(export_directory)) setwd(export_directory)

  if(return.R){
    returnlist <- list()
    if(length(grep("ref",typeof))==1) returnlist$ref <- ref$merge
    if(length(grep("drk",typeof))==1) returnlist$drk <- drk$merge
    if(length(grep("spc",typeof))==1) returnlist$spc <- spc$merge
    return(returnlist)
  }
}



