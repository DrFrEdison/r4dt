pls_function.mda <- function(csv_transfered, substance, wlr, ncomp, segments = 5, spc = c("spc", "1st", "2nd"), validation = "none", alpha = 0.05){
  
  pls <- list() 
  
  csv_transfered$spc <- t(csv_transfered$spc)
  csv_transfered$spc1st <- t(csv_transfered$spc1st)
  csv_transfered$spc2nd <- t(csv_transfered$spc2nd)
  
  substance_column <- grep(substance,colnames(csv_transfered$data))
  
  suppressWarnings(if(length(substance_column)>1) substance_column <- which(colnames(csv_transfered$data) == substance))
  
  pls$x <- csv_transfered$data[,substance_column]
  pls$xna <- pls$x  
  pls$x <- pls$x[which(!is.na(pls$x))]
  pls$xdat <- csv_transfered$data
  pls$xdat <- pls$xdat[which(!is.na(pls$xna)),]
  
  pls$wlrlist <- list()
  
  if(  !is.data.frame(wlr)) wlr <- data.frame(  do.call(rbind, wlr) )
  if(ncol(wlr) == 1) wlr <- data.frame(t(wlr))
  
  if(nrow(wlr) > 50) messaget <- c(1, seq(1, nrow(wlr), 100)[-1] - 1)
  
  wlrl <- apply(wlr, 1, function(x) c(x[1]:x[2], if(!is.na(x[3])) x[3]:x[4]))
  ifelse(nrow(wlr) > 1
         , wlrlp <- lapply(wlrl, function(x) which(csv_transfered$wl %in% x))
         , wlrlp <- which(csv_transfered$wl %in% wlrl))
  if(nrow(wlr) == 1) wlrlp <- list(wlrlp)
  
  spcl <- as.list(rep(spc, each = length(wlrlp)))
  spcll <- lapply(spcl, function(x) grep(x, names(csv_transfered))[1])
  
  csv_transfered$spc <- t(csv_transfered$spc)
  csv_transfered$spc1st <- t(csv_transfered$spc1st)
  csv_transfered$spc2nd <- t(csv_transfered$spc2nd)
  
  if(segments >= length(pls$x)) segments <- length(pls$x)
  
  pls$pls <- pbmapply(function(x, y) mdatools::pls(csv_transfered[[x]][ , y ], pls$x
                                          , ncomp = 2)
                      , x = spcll
                      , y = wlrlp
                      , SIMPLIFY = F)
  if("spc" %in% spc) pls$spc <- pls$pls[spcl %in% "spc"]
  if("1st" %in% spc) pls$spc1st <- pls$pls[spcl %in% "1st"]
  if("2nd" %in% spc) pls$spc2nd <- pls$pls[spcl %in% "2nd"]
  
  pls$pls <- NULL
  
  if("spc" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)<3)  names(pls$spc)[o] <- paste(wlr[o,1], wlr[o,2], sep="_")
  if("1st" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)<3)  names(pls$spc1st)[o] <- paste(wlr[o,1], wlr[o,2], sep="_")
  if("2nd" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)<3)  names(pls$spc2nd)[o] <- paste(wlr[o,1], wlr[o,2], sep="_")
  
  if("spc" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(is.na(wlr[o,3]))  names(pls$spc)[o] <- paste(wlr[o,1], wlr[o,2], sep="_")
  if("1st" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(is.na(wlr[o,3]))  names(pls$spc1st)[o] <- paste(wlr[o,1], wlr[o,2], sep="_")
  if("2nd" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(is.na(wlr[o,3]))  names(pls$spc2nd)[o] <- paste(wlr[o,1], wlr[o,2], sep="_")
  
  if("spc" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(!is.na(wlr[o,3]))  names(pls$spc)[o] <- paste(wlr[o,1], wlr[o,2], wlr[o,3], wlr[o,4], sep="_")
  if("1st" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(!is.na(wlr[o,3]))  names(pls$spc1st)[o] <- paste(wlr[o,1], wlr[o,2], wlr[o,3], wlr[o,4], sep="_")
  if("2nd" %in% spc) for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(!is.na(wlr[o,3]))  names(pls$spc2nd)[o] <- paste(wlr[o,1], wlr[o,2], wlr[o,3], wlr[o,4], sep="_")
  
  return(pls)
}
