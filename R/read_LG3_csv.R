# read_csv_files_LG3
read.csv.LG3 <- function(firstday
                         , lastday
                         , customer
                         , location
                         , line
                         , product = NA
                         , typecode = NA
                         , export_directory="C://csvtemp"
                         , typeof=c("drk","ref","spc")
                         , slim = T
                         , return.R = F
                         , product_ID = dt$product_ID){

  if(!is.na(product[1])){if(any(product[1] == 0) & location == "Mannheim") product <- "NULL"}
  if(slim == T) coltoremove <- c("lightPath", "ID", "transferFunctionCoef", "location", "unit"
                                 , "measurementTypeCode", "DTproductName"
                                 , "UNSB", "Model", "Corr", "Deviation", "scores", "sT2", "sXS", "ualsL"
                                 , "statusTimestamp", "spectrumTimestamp")

  # create export dir, emtpy lists and setwd ####
  if(!is.na(export_directory)) dir.create(export_directory,showWarnings = F)

  read <- list()

  read$wd.o <- getwd()
  read$wd <- .service_backup_path(customer, location, line)
  setwd(read$wd)

  # name of export files
  read$name$date1 <- gsub("-","",substr(firstday,3,nchar(firstday)))
  read$name$date2 <- gsub("-","",substr(lastday,3,nchar(lastday)))

  product_ID <- data.frame(product_ID)
  product_ID <- product_ID[product_ID$customer == customer,]
  product_ID <- product_ID[product_ID$location == location,]
  if(nchar(unique(product_ID$line)[1]) != 0)   product_ID <- product_ID[product_ID$line == line,]
  read$name$product <- unique(product_ID$beverage[which(product_ID$ID==product[1])])

  read$name$file <- paste(read$name$date1, read$name$date2, location, line, read$name$product, paste(product, collapse = "_"), sep = "_")

  # get production df for each day
  if(!is.na(product[1])){
    read$line.product.date <- do.call(rbind, lapply(dir(pattern =  paste0(line,".csv")), read.csv2))
    read$line.product.date <- read$line.product.date[which(nchar(as.character(read$line.product.date$Date)) == 10),]
    read$line.product.date <- read$line.product.date[which(read$line.product.date$Date >= firstday & read$line.product.date$Date <= lastday) ,]
    read$line.product.date <- read$line.product.date[which(read$line.product.date$Produkt_1 %in% product),]

    if(nrow(read$line.product.date) == 0){
      message("No files found with product number ", product)
      stop_quietly() # stop(paste0("No files found with product number", product), call. = F, domain = NA)
    }
  }

  # Reference Spectra ####
  if(any(grepl( "ref", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./ref")

    ref <- list()

    # list ref files
    ref$files <- dir(pattern = "_ref.csv")[which(substr(dir(pattern = "_ref.csv"), 1, 10)>=firstday & substr(dir(pattern = "_ref.csv"), 1, 10)<=lastday)]

    # read  and merge ref files
    if(!is.na(product[1])){ ref$merge <- lapply(ref$files[substr(ref$files, 1, 10) %in% read$line.product.date$Date] ,function(x) fread(x, dec = ","))
    } else{ ref$merge <- lapply(ref$files,function(x) fread(x, dec = ","))}

    ref$merge <-  rbindlist(ref$merge, fill = T)

    # filter by product
    if(!is.na(product[1])) ref$merge <- ref$merge[ref$merge$MixerNumber %in% product,]

    ref$merge$date <- as.POSIXct(as.character(ref$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
    ref$merge$datetime <- as.POSIXct(as.character(ref$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
    ref$merge <- ref$merge[, moveme(names(ref$merge), "datetime date time first"), with = F]

    # move wl to last columns if necessary
    if(is.na(as.numeric(gsub("X", "", names(ref$merge)[ncol(ref$merge)])))){
      suppressWarnings(ref$wl <- sort(as.numeric(gsub("X","",names(ref$merge)))))
      for(i in paste0("X", ref$wl)) ref$merge <- ref$merge[ , moveme(names(ref$merge), paste(i, "last")), with = F]}

    if(slim == T) ref$merge <- ref$merge[ , names(ref$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(ref$merge)))))], with = F]
    if(slim == T) ref$merge$date <- NULL
    if(slim == T) ref$merge$time <- NULL

    if(!is.na(export_directory)){
    setwd(export_directory)

    fwrite(x = ref$merge, file = paste0(read$name$file,"_ref.csv"), sep = ";", dec = ",", na = "NA")
    message("Reference Spectra exported")}
    if(!return.R)  rm(ref)
  }

  # Dark Spectra ####
  if(any(grepl( "drk", typeof, fixed = TRUE))){

    setwd(read$wd)
    setwd("./drk")

    drk <- list()

    # list drk files
    drk$files <- dir(pattern = "_drk.csv")[which(substr(dir(pattern = "_drk.csv"), 1, 10)>=firstday & substr(dir(pattern = "_drk.csv"), 1, 10)<=lastday)]

    # read  and merge drk files
    if(!is.na(product[1])){ drk$merge <- lapply(drk$files[substr(drk$files, 1, 10) %in% read$line.product.date$Date] ,function(x) fread(x, dec = ","))
    } else{ drk$merge <- lapply(drk$files,function(x) fread(x, dec = ","))}

    drk$merge <-  rbindlist(drk$merge, fill = T)

    # filter by product
    if(!is.na(product[1])) drk$merge <- drk$merge[drk$merge$MixerNumber %in% product,]

    drk$merge$date <- as.POSIXct(as.character(drk$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
    drk$merge$datetime <- as.POSIXct(as.character(drk$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
    drk$merge <- drk$merge[, moveme(names(drk$merge), "datetime date time first"), with = F]

    # move wl to last columns if necessary
    if(is.na(as.numeric(gsub("X", "", names(drk$merge)[ncol(drk$merge)])))){
      suppressWarnings(drk$wl <- sort(as.numeric(gsub("X","",names(drk$merge)))))
      for(i in paste0("X", drk$wl)) drk$merge <- drk$merge[ , moveme(names(drk$merge), paste(i, "last")), with = F]}

    if(slim == T) drk$merge <- drk$merge[ , names(drk$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(drk$merge)))))], with = F]
    if(slim == T) drk$merge$date <- NULL
    if(slim == T) drk$merge$time <- NULL

    if(!is.na(export_directory)){
          setwd(export_directory)

    fwrite(x = drk$merge, file = paste0(read$name$file,"_drk.csv"), sep = ";", dec = ",", na = "NA")
    message("Dark Spectra exported")}
    if(!return.R)  rm(drk)
  }


  # Production Spectra ####
  if(any(grepl( "spc", typeof, fixed = TRUE))){
    gc()
    setwd(read$wd)
    setwd("./spc")

    spc <- list()

    # list spc files
    spc$files <- dir(pattern = "_spc.csv")[which(substr(dir(pattern = "_spc.csv"), 1, 10)>=firstday & substr(dir(pattern = "_spc.csv"), 1, 10)<=lastday)]

    # read  and merge spc files
    if(!is.na(product[1])){ spc$merge <- lapply(spc$files[substr(spc$files, 1, 10) %in% read$line.product.date$Date] ,function(x) fread(x, dec = ","))
    } else{ spc$merge <- lapply(spc$files,function(x) fread(x, dec = ","))}

    # filter by product
    if(!is.na(product[1])) spc$merge <- lapply(spc$merge, function(x) x[x$MixerNumber %in% product,])

    # filter by typecode
    if(!is.na(typecode[1])) spc$merge <- lapply(spc$merge, function(x) x[x$measurementTypeCode %in% typecode,])

    # merge
    spc$merge <-  rbindlist(spc$merge, fill = T)

    if(slim == F) spc$merge$date <- as.POSIXct(as.character(spc$merge$date),format="%Y-%m-%d",tz="Europe/Berlin")
    spc$merge$datetime <- as.POSIXct(as.character(spc$merge$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")

    if(slim == F) spc$merge <- spc$merge[ , moveme(names(spc$merge), "datetime date time first"), with = F]
    if(slim == T) spc$merge <- spc$merge[ ,moveme(names(spc$merge), "datetime first"), with = F]

    if(slim == T) spc$merge <- spc$merge[ , names(spc$merge)[-unique(unlist(lapply(coltoremove, function(x) grep(x, names(spc$merge)))))], with = F]
    if(slim == T) spc$merge$date <- NULL
    if(slim == T) spc$merge$time <- NULL

    # move wl to last columns if necessary
    if(is.na(as.numeric(gsub("X", "", names(spc$merge)[ncol(spc$merge)])))){
      suppressWarnings(spc$wl <- sort(as.numeric(gsub("X","",names(spc$merge)))))
      for(i in paste0("X", spc$wl)) spc$merge <- spc$merge[ , moveme(names(spc$merge), paste(i, "last")), with = F]}

    # spc$merge[ , (which(names(spc$merge) == "accumulations") + 1) : (which(names(spc$merge) == "190") - 1)] <- apply(spc$merge[ , (which(names(spc$merge) == "accumulations") + 1) : (which(names(spc$merge) == "190") - 1)], 2, as.numeric)
    spc$merge <- spc$merge[ , !sapply(spc$merge, function(x) all(is.na(x))), with = F]

    setwd(export_directory)

    if(!is.na(export_directory)){
      fwrite(x = spc$merge, file = paste0(read$name$file,"_spc.csv"), sep = ";", dec = ",", na = "NA", dateTimeAs = "ISO")
    message("Production Spectra exported")}
  }

  if(!is.na(export_directory)) message(paste("Export of .csv files to",export_directory, "finished"))
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
