transfer_csv.num.col <- function(csv.file){
  numcol <- suppressWarnings(as.numeric(gsub("X", "",  colnames(csv.file))))
  wavelength <-  numcol[suppressWarnings(which( !is.na(numcol) & numcol > 100))]
  numcol <- suppressWarnings(which( !is.na(numcol) & numcol > 100))
  returnlist <- list(numcol, wavelength)
  names(returnlist) <- c("numcol", "wl")
  return(returnlist)
  }

	
csv.file <- beverage$raw$spc$Mannheim_MY[ 1:50000 , ]


transfer_csv <- function(csv.file # input csv file
                         , p = 2 # polynomial order for derivative
                         , n1 = 7 # window size for 1st derivative
                         , n2 = 11 # window size for 2nd derivative
                         , derivative="all" # calculate derivatives or not: "all" or NA
                         , tz = "UTC"
                         , data.table.set = T){
  
  if(!is.data.table(csv.file)) data.table.set = F
  # extract spectra columns by searching for numeric column names and number > 100
  numcol <- suppressWarnings(as.numeric(gsub("X", "",  colnames(csv.file))))
  if(!data.table.set) csv.file.spc <- csv.file[ , suppressWarnings(which( !is.na(numcol) & numcol > 100))]
  if(data.table.set) csv.file.spc <- csv.file[ , suppressWarnings(which( !is.na(numcol) & numcol > 100)), with = F]
  
  # Extract wavelengths
  wavelength <-  numcol[suppressWarnings(which( !is.na(numcol) & numcol > 100))]
  
  # extract data columns by excluding numeric column names or number < 100
  if(!data.table.set) csv.file.data <- csv.file[ , suppressWarnings(which( is.na(numcol) | numcol < 100))]
  if(data.table.set) csv.file.data <- csv.file[ , suppressWarnings(which( is.na(numcol) | numcol < 100)), with = F]
  
  # change date & time format
  if(any(grepl( "datetime" , names(csv.file.data), fixed = TRUE))) if(unlist(gregexpr("T", substr(csv.file.data$datetime[1], 1, 19)))>0) csv.file.data$datetime <- as.POSIXct(strptime(csv.file.data$datetime, "%Y-%m-%dT%H:%M:%S") )
  if(any(grepl( "datetime" , names(csv.file.data), fixed = TRUE))) csv.file.data$datetime <- as.POSIXct(csv.file.data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if(any(grepl( "date" , names(csv.file.data), fixed = TRUE))) csv.file.data$date <- as.POSIXct(csv.file.data$date, format = "%Y-%m-%d", tz = tz)
  if(any(grepl( "time" , names(csv.file.data), fixed = TRUE))){
    
    csv.file.data$time <- strftime(csv.file.data$datetime, format="%H:%M:%S")
  }
  
  if(!is.na(derivative)){
    
    # make columns numeric
    if(!any(apply(csv.file.spc, 2, is.numeric)))  csv.file.spc <- apply(csv.file.spc , 2, function(x) as.numeric(as.character(x)))
    
    # first derivative
    
    if(!is.data.table( csv.file.spc ))
      csv.file.spc.1st <- apply(t(csv.file.spc),2,
                                function(x) savitzkyGolay(x, m = 1, p = p, w = n1))
    
    if(is.data.table( csv.file.spc ))
      csv.file.spc.1st <- data.table( csv.file.spc[, savitzkyGolay(.SD, m = 1, p = p, w = n1)] )
    
    # second derivative
    if(!is.data.table( csv.file.spc ))
      csv.file.spc.2nd <- apply(t(csv.file.spc),2,
                                function(x) savitzkyGolay(x, m = 2, p = p, w = n2))
    
    if(is.data.table( csv.file.spc ))
      csv.file.spc.2nd <- data.table( csv.file.spc[, savitzkyGolay(.SD, m = 2, p = p, w = n2)] )

    # add zero columns to matrix to ensure homogenous number of columns depending on the window size
    if(!is.data.table( csv.file.spc )){ matrixtoadd <- matrix(data=0, nrow = (n1-1)/2, ncol = nrow(csv.file.spc))
    csv.file.spc.1st <- rbind(matrixtoadd, csv.file.spc.1st, matrixtoadd)}
    if(is.data.table( csv.file.spc )){
      for(i in 1:(length(wavelength[ !wavelength %in% gsub("X", "", colnames(csv.file.spc.1st)) ]) / 2))
      csv.file.spc.1st <- cbind(0 , csv.file.spc.1st)
    for(i in 1:(length(wavelength[ !wavelength %in% gsub("X", "", colnames(csv.file.spc.1st)) ]) / 2))
      csv.file.spc.1st <- cbind(csv.file.spc.1st, 0)
    }
    
    # add zero columns to matrix to ensure homogenous number of columns depending on the window size
    if(!is.data.table( csv.file.spc )){ matrixtoadd <- matrix(data=0, nrow = (n2-1)/2, ncol = nrow(csv.file.spc))
    csv.file.spc.2nd <- rbind(matrixtoadd, csv.file.spc.2nd, matrixtoadd)}
    if(is.data.table( csv.file.spc )){
      for(i in 1:(length(wavelength[ !wavelength %in% gsub("X", "", colnames(csv.file.spc.2nd)) ]) / 2))
        csv.file.spc.2nd <- cbind(0 , csv.file.spc.2nd)
      for(i in 1:(length(wavelength[ !wavelength %in% gsub("X", "", colnames(csv.file.spc.2nd)) ]) / 2))
        csv.file.spc.2nd <- cbind(csv.file.spc.2nd, 0)
    }
    
    if(!is.data.table( csv.file.spc )){
    # data.frame
    csv.file.spc.1st <- data.frame(csv.file.spc.1st)
    csv.file.spc.2nd <- data.frame(csv.file.spc.2nd)
    
    # transpose
    csv.file.spc.1st <- t(csv.file.spc.1st)
    csv.file.spc.2nd <- t(csv.file.spc.2nd)
    }
    
    # homogenize column names
    colnames(csv.file.spc) <- paste0("X", wavelength)
    colnames(csv.file.spc.1st) <- colnames(csv.file.spc)
    colnames(csv.file.spc.2nd) <- colnames(csv.file.spc)
  }
  
  # make columns numeric
  if(is.data.frame(csv.file.data)){
    nums <- suppressWarnings(as.numeric(which(lapply(apply(apply(csv.file.data, 2, as.numeric), 2, function(x) unique(!is.na(x))), any) == T)))
    if(!data.table.set) if(length(nums)>1) csv.file.data[,nums] <- apply(csv.file.data[,nums],2,function(x) as.numeric(as.character(gsub(",",".",x))))
    if(!data.table.set) if(length(nums)==1) csv.file.data[,nums] <- as.numeric(as.character(gsub(",",".",csv.file.data[,nums])))
    if(data.table.set) csv.file.data[,names(csv.file.data)[nums]] <- csv.file.data[, lapply(.SD, function(x) as.numeric(as.character(gsub(",", ".", x)))), .SDcols = c(nums)]
    
  }
  
  # name list
  if(!is.na(derivative)){csv.filelist <- list(csv.file.data,wavelength,csv.file.spc,csv.file.spc.1st,csv.file.spc.2nd)} else {
    csv.filelist <- list(csv.file.data,wavelength,csv.file.spc)}
  
  if(!is.na(derivative)){names(csv.filelist) <- c("data","wl","spc","spc1st","spc2nd")} else {
    names(csv.filelist) <- c("data","wl","spc")}
  
  # make data and spectra as data.frame
  # if(!data.table.set)
  # for(i in which(!names(csv.filelist) %in% "wl")) csv.filelist[[i]] <- data.frame(csv.filelist[[i]])
  # 
  return(csv.filelist)
}


transfer_csv_status <- function(csv_status_file){
names(csv_status_file) <- c("datetime", "Pressure", "Flow", "TempFluid", "TempSPC", "TempRack", "TempAmbient")
csv_status_file$datetime <- as.POSIXct(csv_status_file$datetime)
csv_status_file$time <- strftime(csv_status_file$datetime, format = "%H:%M:%S")
csv_status_file$date <- as.Date(csv_status_file$datetime)

csv_status_file$Pressure <- as.numeric(gsub(",", ".", csv_status_file$Pressure))
csv_status_file$Flow <- as.numeric(gsub(",", ".", csv_status_file$Flow))
csv_status_file$TempFluid <- as.numeric(gsub(",", ".", csv_status_file$TempFluid))
csv_status_file$TempSPC <- as.numeric(gsub(",", ".", csv_status_file$TempSPC))
csv_status_file$TempRack <- as.numeric(gsub(",", ".", csv_status_file$TempRack))
csv_status_file$TempAmbient <- as.numeric(gsub(",", ".", csv_status_file$TempAmbient))

csv_status_file <- csv_status_file[ , moveme(names(csv_status_file), "datetime date time first")]
csv_status_file <- csv_status_file[order(csv_status_file$datetime) , ]

  return(csv_status_file)
}
