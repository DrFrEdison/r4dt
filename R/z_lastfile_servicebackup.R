lastfile_serverbackup <- function(customer){
  
  dirlist <- list()
  dirlist$customer <- names(wd$servicebackup)[-c(1:2)]
  
  if(customer == "CCEP"){
    dirlist$dirs$CCEP <- list.dirs(wd$servicebackup$CCEP[[1]], recursive = T)
    dirlist$files$CCEP <- lapply(dirlist$dirs$CCEP[grep("spc", dirlist$dirs$CCEP)], function(x) list.files(x, pattern = ".csv$"))
    dirlist$max$CCEP <- lapply(dirlist$files$CCEP, function(x) x[which.max(as.Date(substr(x, 1, 10)))])
  }
  
  if(customer == "MEG"){
    dirlist$dirs$MEG <- list.dirs(wd$servicebackup$MEG[[1]], recursive = T)
    dirlist$files$MEG <- lapply(dirlist$dirs$MEG[grep("spc", dirlist$dirs$MEG)], function(x) list.files(x, pattern = ".csv$"))
    dirlist$max$MEG <- lapply(dirlist$files$MEG, function(x) x[which.max(as.Date(substr(x, 1, 10)))])
  }
  
  if(customer == "Pepsi"){
    dirlist$dirs$Pepsi <- list.dirs(wd$servicebackup$Pepsi[[1]], recursive = T)
    dirlist$files$Pepsi <- lapply(dirlist$dirs$Pepsi[grep("spc", dirlist$dirs$Pepsi)], function(x) list.files(x, pattern = ".csv$"))
    dirlist$max$Pepsi <- lapply(dirlist$files$Pepsi, function(x) x[which.max(as.Date(substr(x, 1, 10)))])
  }
  
  return(dirlist$max[grep(customer, names(dirlist$max))])
}