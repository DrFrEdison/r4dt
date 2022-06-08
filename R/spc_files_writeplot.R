write_spc_files <- function(spc_file, baseline = F, write = F, filename = "export", path = NA, return_R = T){

  if(baseline == F) spctoadd <- do.call(rbind.fill, lapply(lapply(spc_file$spc, function(x) rbind.fill(x)), function(x) data.frame(t(x))))
  if(baseline == T) spctoadd <- do.call(rbind.fill, lapply(lapply(spc_file$spc_baseline, function(x) rbind.fill(x)), function(x) data.frame(t(x))))

  for(i in sort(as.numeric(gsub("X", "", colnames(spctoadd)))))
    spctoadd <- spctoadd[ , moveme(colnames(spctoadd), paste0(colnames(spctoadd)[grep(i, colnames(spctoadd))], " last"))]

  require(lubridate)
  tzp <- unique(unlist(lapply(spc_file$data, function(x) tz(x$fdate))))

  Iterations <- lapply(spc_file$data, function(x) unique(as.numeric(as.character(gsub(",", ".", x$It))) / 1000))

  if(length(grep("Interval", names(spc_file$data[[1]]))) > 0)  Interval <- lapply(spc_file$data, function(x) as.character(x$Interval))
  if(length(grep("Interval", names(spc_file$data[[1]]))) == 0)  Interval <- 0

  if(unique(substr(Interval[[1]], nchar(Interval[[1]]), nchar(Interval[[1]]))) == "s") Interval <- lapply(Interval, function(x) as.numeric(gsub(",", ".", substr(x, 1, nchar(x) - 1))))

  Interval <- mapply(function(x, y) x + y
                     , x = Interval
                     , y = Iterations)

  Interval <- lapply(Interval, function(x) cumsum(x) - x[1])

  datetimep <- lapply(spc_file$data, function(x) as.POSIXct(as.character(x$fdate), tz = tzp))

  if(sum(unique(unlist(Interval)), na.rm = T) != 0)  datetimep <-   mapply(function(x, y) x + y
                                                           , x = datetimep
                                                           , y = Interval)

  datetimep <- as.POSIXct(unlist(lapply(datetimep, as.character)), tz = tzp, format = "%Y-%m-%d %H:%M:%S")

  date <- as.Date(datetimep, tz = tzp)
  time = strftime(with_tz(datetimep, tzone = tzp), format = "%H:%M:%S", tz = tzp)

  Iterations = unlist(lapply(spc_file$data, function(x) x$It))
  Average = unlist(lapply(spc_file$data, function(x) x$Aver))
  filenamep = unlist(lapply(spc_file$data, function(x) as.character(basename(x$filename))))

  if(is.null(datetimep)) datetimep <- NA
  if(is.null(date)) date <- NA
  if(is.null(time)) time <- NA
  if(is.null(Iterations)) Iterations <- NA
  if(is.null(Average)) Average <- NA
  if(is.null(filename)) filenamep <- NA

  toexport <- data.frame(datetimep, date, time, Iterations, Average, filenamep, spctoadd)

  toexport$Iterations <- as.numeric(as.character(gsub(",",".",toexport$Iterations)))
  toexport$Average <- as.numeric(as.character(gsub(",",".",toexport$Average)))

  suppressWarnings(
    names(toexport)[which(!is.na(as.numeric(gsub("X", "", names(toexport)))))] <-
      wl <- gsub("X","",names(toexport)[which(!is.na(as.numeric(gsub("X", "", names(toexport)))))])
  )
  wl <- as.numeric(wl)
  toexport <- toexport[order(toexport$datetimep) , ]

  difftime <- cumsum(c(0, diff(toexport$datetimep)))
  difftimeraw <- c(0, diff(toexport$datetimep))

  if(sum(unique(unlist(Interval)), na.rm = T) != 0)  toexport <- cbind(toexport, difftime_sec = difftimeraw, difftime_cum = difftime)

  toexport <- toexport[ , moveme(names(toexport), "Iterations Average filenamep last")]
  for(i in wl) toexport <- toexport[ , moveme(names(toexport), paste0(i, " last"))]

  if(is.na(path)) path <- paste0(getwd(),"/")
  if(write == T) write.csv2(toexport, paste0(path,"/", filename,".csv"), row.names = F)

  if(return_R ==T) return(toexport)
}


