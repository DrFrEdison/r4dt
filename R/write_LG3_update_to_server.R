
LG3_transform_SQL_update_to_csv <- function(csv_file,
                                     file_directory,
                                     wl = 190:598,
                                     export_server = T,
                                     sqlquestion = NA){
  setwd(file_directory)
  csv_filep <- fread(csv_file, nrows = 200, fill = T)

  if(ncol(csv_filep) > 600) sqlquestion <- "drk_ref"
  if(ncol(csv_filep) < 600) sqlquestion <- "production"

  # Column names of your SQL-Export file
  colnames_sql <- list()
  colnames_sql$production <- names(fread(paste0(wd$sql$LG3_spec[[1]], "MessdatenSpalten.csv")))
  colnames_sql$drk_ref <- names(fread(paste0(wd$sql$LG3_spec[[1]], "BackgrounsSpectraSpalten.csv")))

  colnames_sql$production <- c("location", "line", colnames_sql$production)
  colnames_sql$drk_ref <- c("location", "line", colnames_sql$drk_ref)

  setwd(file_directory)

  # sql list
  sql <- list()
  sql <- lapply(csv_file, function(x) fread(x, dec = ",", sep = ";", fill = T))
  # sql <- lapply(csv_file,function(x) read.csv2(x,head=F,sep=";",dec=",",fileEncoding = "UTF-8-BOM", check.names = F, colClasses = "character"))

  customer <- "CCEP"
  location <- unique(sql[[1]][ , 1])

  if(location == "DOR") location = "Dorsten"
  if(location == "MAN") location = "Mannheim"
  if(location == "MOG") location = "Moenchengladbach"

  line <- as.character(unique(sql[[1]][ , 2]))

  export_directory <- .service_backup_path(customer,location,line)

  # rbind ####
  sql_raw <- rbindlist(sql)
  sql_raw <- sql_raw[,-ncol(sql_raw),with = F]

  yearp <- unique(sql_raw[, lapply(.SD, function(x) year(as.POSIXct(as.character(x)))), .SDcols = c(4)])

  # search for # ####
  if(sqlquestion %in% "drk_ref") colnames(sql_raw) <- c(colnames_sql$drk_ref[-c((length(colnames_sql$drk_ref)-2) : length(colnames_sql$drk_ref))]
                                                         , wl
                                                         , "toremove"
                                                         , "timestamp_ref"
                                                         , wl)

  if(sqlquestion %in% "production") colnames(sql_raw) <- c(colnames_sql$production[-length(colnames_sql$production)],rep(wl,length(sqlquestion)))

  sql_raw$lightPath <- as.numeric(sql_raw$lightPath)
  # order by date ####
  sql_raw <- sql_raw[order(sql_raw$timestamp),]

  # make columns numeric ####
  sqlcomma <- apply( sql_raw[ round(seq(1, nrow(sql_raw), len = nrow(sql_raw) / 5) , 0) , ], 2, function(x) grep( ",", x))
  cols <- as.numeric(which( unlist( lapply(sqlcomma, length) ) > 0 ))

  suppressWarnings(  sql_raw <- sql_raw[ , ( cols ) := lapply(.SD, function(x) as.numeric( as.character( gsub( ",", ".", x)))), .SDcols = cols] )

  # remove and wl ####
  if(sqlquestion %in% "drk_ref") sql_raw$toremove <- NULL

  sql$data <- sql_raw[,which(!colnames(sql_raw) %in% wl), with = F]

  # split data
  if(length(sqlquestion)==1 && sqlquestion %in% "production") sql$spc <- sql_raw[,which(colnames(sql_raw) %in% wl), with = F]
  if(length(sqlquestion)==1 && sqlquestion %in% "drk_ref") sql$drk <- sql_raw[,which(colnames(sql_raw) %in% wl)[1:length(wl)], with = F]
  if(length(sqlquestion)==1 && sqlquestion %in% "drk_ref") sql$ref <- sql_raw[ , (which(colnames(sql_raw) == "timestamp_ref") + 1) : ncol(sql_raw), with = F]
  rm(sql_raw)

  sql$data$datetime <- sql$data$timestamp
  sql$data$date <- as.Date(sql$data$datetime, tz = "UTC")
  sql$data$time <- strftime(sql$data$datetime, format = "%H:%M:%S", tz = "UTC")
  sql$data <- sql$data[, moveme(names(sql$data), "time first; date first; datetime first; line first; location first"), with = F]
  sql$data$timestamp <- NULL

  if(sqlquestion %in% "drk_ref") sql$data$statusTimestamp <- NULL

  if(length(sqlquestion)==1 && sqlquestion %in% "production") sql$spc <- t(sql$spc)
  if(length(sqlquestion)==1 && sqlquestion %in% "drk_ref") sql$drk <- t(sql$drk)
  if(length(sqlquestion)==1 && sqlquestion %in% "drk_ref") sql$ref <- t(sql$ref)

  # export ####
  if(length(which(sqlquestion=="drk_ref"))==1){
    refdatamean <- apply(sql$ref,2,mean)
    refdatasd <- apply(sql$ref,2,sd)
    if(length(which(!duplicated(refdatamean)))!=length(which(!duplicated(refdatasd)))){
      if(length(which(!duplicated(refdatamean)))>length(which(!duplicated(refdatasd)))){ dupref <- which(!duplicated(refdatasd)) } else{ dupref <- which(!duplicated(refdatamean))}
    } else dupref <- as.numeric(which(!duplicated(refdatamean)))

    refdata <- sql$data[dupref,]
    # refdata$datetime <- refdata$timestamp_ref
    refdata$timestamp_ref <- NULL
    refdata$spectrumTimestampDRK <- NULL

    exportref <- t(sql$ref[,dupref]);rm(refdatamean,refdatasd,dupref)
    suppressWarnings(export_ref <- data.table(refdata,exportref))
    suppressWarnings(colnames(export_ref)[which(!is.na(as.numeric(gsub("X","",colnames(export_ref)))))] <- paste0("X", wl))

    drkdatamean <- apply(sql$drk,2,mean)
    drkdatasd <- apply(sql$drk,2,sd)
    if(length(which(!duplicated(drkdatamean)))!=length(which(!duplicated(drkdatasd)))){
      if(length(which(!duplicated(drkdatamean)))>length(which(!duplicated(drkdatasd)))){ dupdrk <- which(!duplicated(drkdatasd)) } else{ dupdrk <- which(!duplicated(drkdatamean))}
    } else dupdrk <- as.numeric(which(!duplicated(drkdatamean)))

    drkdata <- sql$data[dupdrk,]
    # drkdata$datetime <- drkdata$spectrumTimestampDRK
    drkdata$timestamp_ref <- NULL
    drkdata$spectrumTimestampDRK <- NULL

    exportdrk <- t(sql$drk[,dupdrk]);rm(drkdatamean,drkdatasd,dupdrk)
    suppressWarnings(export_drk <- data.frame(drkdata,exportdrk))
    suppressWarnings(colnames(export_drk)[which(!is.na(as.numeric(gsub("X","",colnames(export_drk)))))] <- paste0("X", wl))
    }

  if(length(which(sqlquestion=="production"))==1){
    export_spc <- data.table(sql$data,t(sql$spc))

    suppressWarnings(colnames(export_spc)[which(!is.na(as.numeric(gsub("X","",colnames(export_spc)))))] <- paste0("X", wl))
  }

  if(export_server==T){
    if(length(which(sqlquestion=="drk_ref"))==1){
      for(i in 1:length(unique(export_ref$date))){
        reftoexport <- export_ref[which(export_ref$date==unique(export_ref$date)[i]),]
        setwd(export_directory)
        setwd("./ref")
        if(length(which(substr(dir(),1,10)==unique(reftoexport$date)))>0){
          reftomerge <- fread(dir()[which(gsub("_ref.csv","",dir())==unique(reftoexport$date))], sep = ";", dec = ",")

          reftomerge$date <- as.Date(reftomerge$date)
          # attr(reftomerge$datetime, "tzone") <- "Europe/Berlin"

          ifelse(length(unique(names(reftomerge)!=names(reftoexport)))>1,
                 refmerged <- rbindlist(list(reftomerge,reftoexport), fill = T),
                 refmerged <- rbindlist(list(reftomerge,reftoexport)))

          refmerged <- refmerged[order(refmerged$datetime),]
          refmerged <- refmerged[which(!duplicated(reftomerge$datetime)),]

          reftoexport <- refmerged
          if(is.na(as.numeric(gsub("X", "", names(reftoexport)[ncol(reftoexport)])))){
            suppressWarnings(refwl <- sort(as.numeric(gsub("X","",names(reftoexport)))))
            for(i in paste0("X", refwl)) reftoexport <- reftoexport[ , moveme(names(reftoexport), paste(i, "last")), with = F]}
        }
        fwrite(reftoexport,paste0(unique(reftoexport$date),"_",
                                      customer,"_",
                                      location,"_",
                                      line,"_ref.csv"), sep = ";", dec = ",")
      }
      message(paste("ref exported to",getwd()))
    }

    if(length(which(sqlquestion=="drk_ref"))==1){
      for(i in 1:length(unique(export_drk$date))){
        drktoexport <- export_drk[which(export_drk$date==unique(export_drk$date)[i]),]
        setwd(export_directory)
        setwd("./drk")
        if(length(which(substr(dir(),1,10)==unique(drktoexport$date)))>0){
          drktomerge <- fread(dir()[which(gsub("_drk.csv","",dir())==unique(drktoexport$date))], sep = ";", dec = ",")

          drktomerge$date <- as.Date(drktomerge$date)
          # attr(drktomerge$datetime, "tzone") <- "Europe/Berlin"

          ifelse(length(unique(names(drktomerge)!=names(drktoexport)))>1,
                 drkmerged <- rbindlist(list(drktomerge,drktoexport), fill = T),
                 drkmerged <- rbindlist(list(drktomerge,drktoexport)))

          drkmerged <- drkmerged[order(drkmerged$datetime),]
          drkmerged <- drkmerged[which(!duplicated(drktomerge$datetime)),]

          drktoexport <- drkmerged
          if(is.na(as.numeric(gsub("X", "", names(drktoexport)[ncol(drktoexport)])))){
            suppressWarnings(drkwl <- sort(as.numeric(gsub("X","",names(drktoexport)))))
            for(i in paste0("X", drkwl)) drktoexport <- drktoexport[ , moveme(names(drktoexport), paste(i, "last")), with = F]}
        }

        fwrite(drktoexport,paste0(unique(drktoexport$date),"_",
                                  customer,"_",
                                  location,"_",
                                  line,"_drk.csv"), sep = ";", dec = ",")
      }
      message(paste("drk exported to",getwd()))
    }

    if(length(which(sqlquestion=="production"))==1){
      for(i in 1:length(unique(export_spc$date))){
        spctoexport <- export_spc[which(export_spc$date==unique(export_spc$date)[i]),]
        setwd(export_directory)
        setwd("./spc")
        if(length(which(substr(dir(),1,10)==unique(spctoexport$date)))>0){
          spctomerge <- fread(dir()[which(gsub("_spc.csv","",dir())==unique(spctoexport$date))], sep = ";", dec = ",")

          spctomerge$date <- as.Date(spctomerge$date)
          # attr(spctomerge$datetime, "tzone") <- "Europe/Berlin"

          ifelse(length(unique(names(spctomerge)!=names(spctoexport)))>1,
                 spcmerged <- rbindlist(list(spctomerge,spctoexport), fill = T),
                 spcmerged <- rbindlist(list(spctomerge,spctoexport)))

          spcmerged <- spcmerged[order(spcmerged$datetime),]
          spcmerged <- spcmerged[which(!duplicated(spctomerge$datetime)),]

          spctoexport <- spcmerged
          if(is.na(as.numeric(gsub("X", "", names(spctoexport)[ncol(spctoexport)])))){
            suppressWarnings(spcwl <- sort(as.numeric(gsub("X","",names(spctoexport)))))
            for(i in paste0("X", spcwl)) spctoexport <- spctoexport[ , moveme(names(spctoexport), paste(i, "last")), with = F]}
        }
        fwrite(spctoexport,paste0(unique(spctoexport$date),"_",
                                      customer,"_",
                                      location,"_",
                                      line,"_spc.csv"), sep = ";", dec = ",")
      }
      message(paste("spc exported to",getwd()))
    }

    if(location == "DOR") location = "Dorsten"
    if(location == "MAN") location = "Mannheim"
    if(location == "MOG") location = "Moenchengladbach"

    for(k in as.character(unique(sql$data$date)))
    if(length(which(sqlquestion=="production"))==1) produkt_per_day_year(customer = customer
                                                                         , location = location
                                                                         , line = line
                                                                         , LG = "3"
                                                                         , year = yearp
                                                                         , date = k)

  }
}
