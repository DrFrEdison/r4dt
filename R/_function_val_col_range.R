spectra.validation.col <- function(val.vector
                                   , val.levels = c("valid", "critical", "invalid", "empty")
                                   , val.col = c("darkgreen", "orange", "red", "white")){
  val.vector <- factor(val.vector, levels = val.levels)
  val.col <- val.col[val.vector]
  return(val.col)
}

spectra.validation.range <- function(valid.vector, drkref.datetime, spc.datetime, pattern = c("empty", "critical", "invalid")){
  if( length(which(valid.vector %in% pattern)) == 0) return(1:length(spc.datetime))
  badspc.1 <- drkref.datetime[ which(valid.vector %in% pattern) ]
  badspc.2 <- drkref.datetime[ which(valid.vector %in% pattern) + 1 ]
  badspc.2[which(is.na(badspc.2))] <- max(spc.datetime)
  badrange <- mapply(function(badspc.1, badspc.2) which(spc.datetime > badspc.1 & spc.datetime < badspc.2)
                     , badspc.1 = badspc.1
                     , badspc.2 = badspc.2)
  badrange <- unique(sort(unlist(badrange)))
  
  goodrange <- which(!1:length(spc.datetime) %in% badrange)
  return(goodrange)
}
