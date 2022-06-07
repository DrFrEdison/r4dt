pls_function_lin <- function(csv_transfered, substance, lin_matrix, segments = 40){
  
  csv_transfered = seqp$model[[1]]
  
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
  
  wlr <- lin_matrix[ , grep("wl", colnames(lin_matrix))]
  ncomp <- lin_matrix$ncomp
  spc <- lin_matrix$spc
  
  if(nrow(wlr) > 50) messaget <- c(1, seq(1, nrow(wlr), 100)[-1] - 1)
  
  wlrl <- apply(wlr, 1, function(x) c(x[1]:x[2], if(!is.na(x[3])) x[3]:x[4]))
  
  if(!is.null(nrow(wlrl))){  if(nrow(wlrl) > nrow(wlr)) wlrl <- lapply(apply(wlrl, 2, list), unlist) }
  
  ifelse(nrow(wlr) > 1
         , wlrlp <- lapply(wlrl, function(x) which(csv_transfered$wl %in% x))
         , wlrlp <- which(csv_transfered$wl %in% wlrl))
  if(nrow(wlr) == 1) wlrlp <- list(wlrlp)
  
  spcl <- as.list(spc)
  spcll <- lapply(spcl, function(x) grep(x, names(csv_transfered))[1])
  ncompl <- as.list(ncomp)
  
  csv_transfered$spc <- t(csv_transfered$spc)
  csv_transfered$spc1st <- t(csv_transfered$spc1st)
  csv_transfered$spc2nd <- t(csv_transfered$spc2nd)
  
  pls$list <- pbmapply(function(x, y, z) plsr(pls$x ~ csv_transfered[[ x ]][ , y ]
                                            , ncomp = z
                                            , validation = "CV", segments = segments, segment.type = "random", length.seg = 2)
                     , x = spcll
                     , y = wlrlp
                     , z = ncompl
                     , SIMPLIFY = F)
  
  for(o in 1:nrow(wlr)) if(ncol(wlr)<3)  names(pls$list)[o] <- paste(wlr[o,1], wlr[o,2], spc[o], ncomp[o], sep="_")
  for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(is.na(wlr[o,3]))  names(pls$list)[o] <- paste(wlr[o,1], wlr[o,2], spc[o], ncomp[o], sep="_")
  for(o in 1:nrow(wlr)) if(ncol(wlr)>2) if(!is.na(wlr[o,3]))  names(pls$list)[o] <- paste(wlr[o,1], wlr[o,2], wlr[o,3], wlr[o,4], spc[o], ncomp[o], sep="_")
  
  return(pls$list)
}
