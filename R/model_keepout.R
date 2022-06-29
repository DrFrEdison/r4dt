keep.out.unsb <- function(model, wl1, wl2, wl3, wl4){

  ncolp <- ncol(model$data) - 1

  keepout.max <- length(model$wl) + ncolp

  if(wl1 != min(model$wl)){
    keepout.1 <- ncolp + 1
    keepout.2 <- max(which(model$wl < wl1)) + ncolp
  }

  if(wl1 == min(model$wl)){
    keepout.1 = max(which(model$wl < wl2)) + ncolp
  }

  suppressMessages(ifelse(all(!is.na(wl3), wl3 != 0), ttt <- T, ttt <- F))
  if(ttt){
    keepout.3 <- min(which(model$wl > wl2 & model$wl < wl3)) + ncolp
    keepout.4 <- max(which(model$wl > wl2 & model$wl < wl3)) + ncolp

    keepout.5 <- min(which(model$wl > wl4)) + ncolp
  }

  if(ttt){
    keepout.5 <- min(which(model$wl > wl2)) + ncolp
  }

  if(ttt & wl1 != min(model$wl)) keepout.return <- paste0(keepout.1, "-", keepout.2, ",", keepout.3, "-", keepout.4, ",", keepout.5, "-", keepout.max)
  if(!ttt & wl1 != min(model$wl)) keepout.return <- paste0(keepout.1, "-", keepout.2, ",", keepout.5, "-", keepout.max)
  if(!ttt & wl1 == min(model$wl)) keepout.return <- paste0(keepout.1, "-", keepout.max)

  return(keepout.return)
}
