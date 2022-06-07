LG3.status <- function(csv_name, file_encoding = "UTF16"
                       , col_grep = "Time", format_time = "%d.%m.%Y %H:%M:%S"
                       , tz = "Europe/Berlin"){
  
  status <- list()
  
  # read status data
  status$raw <- read.csv2(csv_name, fileEncoding=file_encoding)
  
  # as.character(as.POSIXct time)
  status$raw[ , grep(col_grep, colnames(status$raw))] <- 
    apply(status$raw[ , grep(col_grep, colnames(status$raw))], 2, function(x)
      as.character(as.POSIXct(x, format = format_time, tz = tz)))
  
  # time, parameter and name columns
  status$mod$timecol <- lapply(grep(col_grep, colnames(status$raw)), function(x) status$raw[ , x])
  status$mod$paracol <- lapply(grep(col_grep, colnames(status$raw)), function(x) status$raw[ , x + 1])
  status$mod$paraname <- colnames(status$raw)[grep(col_grep, colnames(status$raw))  + 1]
  
  # make new data.frames
  status$mod$df <- mapply(function(x, y) data.frame(datetime = x, y)
                          , x = status$mod$timecol
                          , y = status$mod$paracol
                          , SIMPLIFY = F)
  
  # give colnames to data.frames
  for(i in 1:length(status$mod$df))
    colnames(status$mod$df[[i]])[2] <- status$mod$paraname[i]
  
  # remove rows with NA as datetime
  status$mod$df <- lapply(status$mod$df, function(x) x[!is.na(x$datetime),])
  
  # merge columns
  status$mod$merge <- status$mod$df[[1]]
  for(i in 2:length(status$mod$df))
    status$mod$merge <- full_join(status$mod$merge, status$mod$df[[i]]
                                  , by = "datetime", copy = T, keep = F)
  
  # clean data
  status$mod$merge <- status$mod$merge[!apply(apply(status$mod$merge[ , -1], 2, is.na), 1, all) , ]
  
  # as.POSIXct time
  status$mod$merge$datetime <- as.POSIXct(status$mod$merge$datetime, tz = tz)
  status$mod$merge <- status$mod$merge[order(status$mod$merge$datetime) , ]
  return(status$mod$merge)
}