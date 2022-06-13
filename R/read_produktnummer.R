produkt_per_day_year <- function(customer, location, line, LG, year, date = NA){
  
  filep <- list()
  filep$year <- as.character(year)
  
  for(k in 1:length(filep$year)){
    
    setwd(service_backup_path(customer, location, line))
    
    setwd("./spc")
    filep$files <- dir(pattern = filep$year[k])
    
    if(!is.na(date)){
      filep$files <- filep$files[grep(date, filep$files)]
    }
    
    if(length(filep$files) < 1) next
    
    for(j in 1:length(filep$files)){
      
      setwd(service_backup_path(customer, location, line))
      setwd("./spc")
      
      # codepages <- setNames(iconvlist(), iconvlist())
      # x <- lapply(codepages, function(enc) try(read.csv2(filep$files[j],
      #                                                     fileEncoding=enc,
      #                                                     nrows=3, header=TRUE)))
      # unique(do.call(rbind, sapply(x, dim)))
      # maybe_ok <- sapply(x, function(x) isTRUE(all.equal(dim(x), c(3,439))))
      # codepages[maybe_ok]
      # x[maybe_ok]
      values = tryCatch(read.csv2(filep$files[j], nrows = 1, fileEncoding="437"),
                        error = function(e) NA, warning = function(w) print('Without semanal file'))
      
      if(is.na(values)[1]) next
      
      # filep$raw <- fread(filep$files[j], nrows = 1)
      # 
      # if(LG != "3"){
      #   filep$colClasses = c(rep(NA , grep("Produkt", names(filep$raw))[1])
      #                        , rep("NULL", c(ncol(filep$raw) - grep("Produkt", names(filep$raw))[1])))}
      # if(LG == "3"){
      #   filep$colClasses = c(rep(NA , grep("MixerNumber", names(filep$raw)))
      #                        , rep("NULL", c(ncol(filep$raw) - grep("MixerNumber", names(filep$raw)))))}
      # 
      filep$trans <- fread(filep$files[j])
      
      if(length(unique(filep$trans[ , grep("^date$", names(filep$trans), ignore.case = T), with = F]))>1) message("More than one date in csv file")
      if(length(unique(filep$trans[ , grep("^date$", names(filep$trans), ignore.case = T), with = F]))>1) break
      
      if(LG != "3") if(length(unique(filep$trans[ , grep("Produkt", names(filep$trans)), with = F])) == 0) next
      if(LG != "3"){
        filep$export <- data.frame(cbind(unique(filep$trans[ , grep("^date$", names(filep$trans), ignore.case = T), with = F]), unique(filep$trans[ , grep("Produkt", names(filep$trans)), with = F])))
        colnames(filep$export) <- c("Date", paste0("Produkt_", (ncol(filep$export) - 1)))
      }
      
      if(LG == "3"){
        filep$export <- data.frame(cbind(unique(filep$trans[ , grep("^date$", names(filep$trans), ignore.case = T), with = F]), unique(filep$trans[ , grep("MixerNumber", names(filep$trans)), with = F])))
        colnames(filep$export) <- c("Date", paste0("Produkt_", (ncol(filep$export) - 1)))
      }
      
      setwd(service_backup_path(customer, location, line))

      filep$name <- paste0(paste(filep$year[k], customer, location, line, sep = "_"),".csv")
      
      if(!filep$name %in% dir()) write.csv2(filep$export, filep$name, row.names = F)
      
      if(filep$name %in% dir()){
        filep$csv_exist <- read.csv2(filep$name)
        filep$csv_exist <- rbind.fill(filep$csv_exist, filep$export)
        
        filep$csv_exist <- filep$csv_exist[which(substr(filep$csv_exist$Date, 1, 4) %in% filep$year[k] == T) , ]
        
        if(any(which(duplicated.data.frame(filep$csv_exist)))) filep$csv_exist <- filep$csv_exist[ -which(duplicated.data.frame(filep$csv_exist)) , ]
        
        filep$csv_exist <- filep$csv_exist[order(filep$csv_exist$Date), ]
        
        write.csv2(filep$csv_exist, filep$name, row.names = F)
      }
    } 
  }
}
