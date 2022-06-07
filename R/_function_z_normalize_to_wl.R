normalize_to_wl <- function(spc_sub, wl_min, wl_max){
  
  if(length(grep("wl", names(spc_sub)) + grep("spc", names(spc_sub))) == 0) transfer = F
  if(length(grep("wl", names(spc_sub)) + grep("spc", names(spc_sub))) != 0) transfer = T
  
  if(transfer == T){
    rangewl <- grep(wl_min, spc_sub$wl) : grep(wl_max, spc_sub$wl)
    spc_sub$wl <- spc_sub$wl[ rangewl ]
    
    spc_sub$spc <- spc_sub$spc[ , rangewl]
    
    if(length(grep("1st", names(spc_sub))) != 0)     spc_sub$spc1st <- spc_sub$spc1st[ , rangewl]
    if(length(grep("2nd", names(spc_sub))) != 0)     spc_sub$spc2nd <- spc_sub$spc2nd[ , rangewl]
  }
    
  if(transfer == F){
    rangewl <- grep(wl_min, names(spc_sub)) : grep(wl_max, names(spc_sub))
    rangedat <- suppressWarnings(which(is.na(as.numeric(gsub("X","",names(spc_sub)))) | as.numeric(gsub("X","",names(spc_sub)))<100))
    
    if(is.data.table(spc_sub)) spc_sub <- spc_sub[ , sort(c(rangedat, rangewl)), with = F]
    if(!is.data.table(spc_sub)) spc_sub <- spc_sub[ , sort(c(rangedat, rangewl))]
  }
   
  return(spc_sub)
}
