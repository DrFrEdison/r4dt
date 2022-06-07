lg_prediction_clean <- function(lg2data, prediction_name, q1 = 1, q2 = 1, ignore_day = NA){
  
  lg2dataraw <- lg2data
  
  ifelse(length(grep("data", names(lg2data))) == 1, tcsv <- T, tcsv <- F)
  if(length(grep("data", names(lg2data))) == 1) lg2data <- lg2data$data
  
  if(length(grep(prediction_name, names(lg2data))) == 0) stop("Parameter not found in lg2data")
  
  clean <- list()
  clean$ycol <- grep(paste0("\\b", prediction_name, "\\b"), names(lg2data))
  
  clean$date <- unique(lg2data$date)
  clean$dat <- list()
  clean$quantile <- list()
  clean$range <- list()
  clean$subdat <- list()
  
  if(length(grep(">", substr(ignore_day, 1, 2))) == 1) ignore_day <- clean$date[which(clean$date > substr(ignore_day, 3, nchar(ignore_day)))]
  if(length(grep(">=", substr(ignore_day, 1, 2))) == 1) ignore_day <- clean$date[which(clean$date >= substr(ignore_day, 3, nchar(ignore_day)))]
  if(length(grep("<", substr(ignore_day, 1, 2))) == 1) ignore_day <- clean$date[which(clean$date < substr(ignore_day, 3, nchar(ignore_day)))]
  if(length(grep("<=", substr(ignore_day, 1, 2))) == 1) ignore_day <- clean$date[which(clean$date <= substr(ignore_day, 3, nchar(ignore_day)))]
  
  for(i in 1:length(clean$date)){
    
    clean$dat[[i]] <- lg2data[which(lg2data$date == clean$date[i]) , clean$ycol]
    clean$quantile[[i]] <- quantile(clean$dat[[i]], na.rm = T)[c(2,4)] * c(q1, q2)
    if(any(as.character(clean$date[i]) %in% ignore_day)) clean$quantile[[i]] <- quantile(clean$dat[[i]], na.rm = T)[c(1,5)]
    clean$range[[i]] <- which(clean$dat[[i]] >= clean$quantile[[i]][1] & clean$dat[[i]] <= clean$quantile[[i]][2])
    clean$subdat[[i]] <- lg2data[ which(lg2data$date == clean$date[i]) , ][clean$range[[i]], ]
    
    if(tcsv == T)clean$subdatspc[[i]] <- lg2dataraw$spc[ which(lg2data$date == clean$date[i]) , ][clean$range[[i]], ]
    if(tcsv == T)clean$subdatspc1st[[i]] <- lg2dataraw$spc1st[ which(lg2data$date == clean$date[i]) , ][clean$range[[i]], ]
    if(tcsv == T)clean$subdatspc2nd[[i]] <- lg2dataraw$spc2nd[ which(lg2data$date == clean$date[i]) , ][clean$range[[i]], ]
  }
  
  if(tcsv == T){
    clean$final$data <- do.call(rbind, clean$subdat)
    clean$final$wl <- lg2dataraw$wl
    clean$final$spc <- do.call(rbind, clean$subdatspc)
    clean$final$spc1st <- do.call(rbind, clean$subdatspc1st)
    clean$final$spc2nd <- do.call(rbind, clean$subdatspc2nd)
  }
  
  if(tcsv == F) clean$final <- do.call(rbind, clean$subdat)
  
  return(clean$final)
}
