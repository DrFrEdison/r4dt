linearitaet_filter <- function(linearitaet_prediction, ncomp, linearitaet_limit_1, linearitaet_limit_2, R_2 = .8, SOLL
                               , pls_merge = NA){

  if("spc" %in% names(linearitaet_prediction)){
    linearitaet_spc <- matrix(nrow = ncol(linearitaet_prediction$spc[[1]]), ncol = ncomp)
    rownames(linearitaet_spc) <- colnames(linearitaet_prediction$spc[[1]])
    colnames(linearitaet_spc) <- 1:ncomp
    if(length(which(is.na(lapply(linearitaet_prediction$spc, sum))))>0)  linearitaet_prediction$spc <- linearitaet_prediction$spc[-which(is.na(lapply(linearitaet_prediction$spc, sum)))]

    spcdiff <- lapply(linearitaet_prediction$spc, function(x) abs(apply(x, 2, function(y) y[length(y)] - y[1])))
    spcR <- lapply(linearitaet_prediction$spc, function(x) apply(x, 2, function(y) cor(SOLL,y)^2))

    spcfilter <- mapply(function(x, y) which(x > linearitaet_limit_1 & x < linearitaet_limit_2 & y > R_2)
                        , x = spcdiff
                        , y = spcR)

    spcdiff <- mapply( function( x , y ) x[y]
                       , x = spcdiff
                       , y = spcfilter)

    spcR <- mapply( function( x , y ) x[y]
                    , x = spcR
                    , y = spcfilter)

    if(length(unlist(spcfilter)) > 0)
      spcfilter <- data.frame(spc = "spc"
                              , wl1 = substr(names(unlist(spcfilter)), 1, 3)
                              , wl2 = substr(names(unlist(spcfilter)), 5, 7)
                              , wl3 = substr(names(unlist(spcfilter)), 9, 11)
                              , wl4 = substr(names(unlist(spcfilter)), 13, 15)
                              , ncomp = rep(1:ncomp, unlist(lapply(spcfilter, length)))
                              , spcdiff = round(unlist(spcdiff),2)
                              , spcR = round(unlist(spcR),2))
  }

  if("spc1st" %in% names(linearitaet_prediction)){
    linearitaet_spc1st <- matrix(nrow = ncol(linearitaet_prediction$spc1st[[1]]), ncol = ncomp)
    rownames(linearitaet_spc1st) <- colnames(linearitaet_prediction$spc1st[[1]])
    colnames(linearitaet_spc1st) <- 1:ncomp
    if(length(which(is.na(lapply(linearitaet_prediction$spc1st, sum))))>0)  linearitaet_prediction$spc1st <- linearitaet_prediction$spc1st[-which(is.na(lapply(linearitaet_prediction$spc1st, sum)))]

    spc1stdiff <- lapply(linearitaet_prediction$spc1st, function(x) abs(apply(x, 2, function(y) y[length(y)] - y[1])))
    spc1stR <- lapply(linearitaet_prediction$spc1st, function(x) apply(x, 2, function(y) cor(SOLL,y)^2))

    spc1stfilter <- mapply(function(x, y) which(x > linearitaet_limit_1 & x < linearitaet_limit_2 & y > R_2)
                           , x = spc1stdiff
                           , y = spc1stR)

    spc1stdiff <- mapply( function( x , y ) x[y]
                          , x = spc1stdiff
                          , y = spc1stfilter)

    spc1stR <- mapply( function( x , y ) x[y]
                       , x = spc1stR
                       , y = spc1stfilter)

    if(length(unlist(spc1stfilter)) > 0)
      spc1stfilter <- data.frame(spc = "1st"
                                 , wl1 = substr(names(unlist(spc1stfilter)), 1, 3)
                                 , wl2 = substr(names(unlist(spc1stfilter)), 5, 7)
                                 , wl3 = substr(names(unlist(spc1stfilter)), 9, 11)
                                 , wl4 = substr(names(unlist(spc1stfilter)), 13, 15)
                                 , ncomp = rep(1:ncomp, unlist(lapply(spc1stfilter, length)))
                                 , spcdiff = round(unlist(spc1stdiff),2)
                                 , spcR = round(unlist(spc1stR),2))
  }

  if("spc2nd" %in% names(linearitaet_prediction)){
    linearitaet_spc2nd <- matrix(nrow = ncol(linearitaet_prediction$spc2nd[[1]]), ncol = ncomp)
    rownames(linearitaet_spc2nd) <- colnames(linearitaet_prediction$spc2nd[[1]])
    colnames(linearitaet_spc2nd) <- 1:ncomp
    if(length(which(is.na(lapply(linearitaet_prediction$spc2nd, sum))))>0)  linearitaet_prediction$spc2nd <- linearitaet_prediction$spc2nd[-which(is.na(lapply(linearitaet_prediction$spc2nd, sum)))]

    spc2nddiff <- lapply(linearitaet_prediction$spc2nd, function(x) abs(apply(x, 2, function(y) y[length(y)] - y[1])))
    spc2ndR <- lapply(linearitaet_prediction$spc2nd, function(x) apply(x, 2, function(y) cor(SOLL,y)^2))

    spc2ndfilter <- mapply(function(x, y) which(x > linearitaet_limit_1 & x < linearitaet_limit_2 & y > R_2)
                           , x = spc2nddiff
                           , y = spc2ndR)

    spc2nddiff <- mapply( function( x , y ) x[y]
                          , x = spc2nddiff
                          , y = spc2ndfilter)

    spc2ndR <- mapply( function( x , y ) x[y]
                       , x = spc2ndR
                       , y = spc2ndfilter)

    if(length(unlist(spc2ndfilter)) > 0)
      spc2ndfilter <- data.frame(spc = "2nd"
                                 , wl1 = substr(names(unlist(spc2ndfilter)), 1, 3)
                                 , wl2 = substr(names(unlist(spc2ndfilter)), 5, 7)
                                 , wl3 = substr(names(unlist(spc2ndfilter)), 9, 11)
                                 , wl4 = substr(names(unlist(spc2ndfilter)), 13, 15)
                                 , ncomp = rep(1:ncomp, unlist(lapply(spc2ndfilter, length)))
                                 , spcdiff = round(unlist(spc2nddiff),2)
                                 , spcR = round(unlist(spc2ndR),2))
  }

  spcfilter <- rbind(
    if("spc" %in% names(linearitaet_prediction)) spcfilter
    , if("spc1st" %in% names(linearitaet_prediction)) spc1stfilter
    , if("spc2nd" %in% names(linearitaet_prediction)) spc2ndfilter)

  spcfilter$wl3[which(spcfilter$wl3 == "")] <- NA
  spcfilter$wl4[which(spcfilter$wl4 == "")] <- NA

  if(unique(unlist(lapply(spcfilter, length))) != 0)  spcfilter <- spcfilter[!duplicated(spcfilter),]

  if( is.data.frame(pls_merge)) if(nrow(pls_merge) == 0) pls_merge = NA

  if(!is.data.frame( pls_merge )){

    if(!is.na(pls_merge)){

      pls_merge <- pls_merge[!duplicated(pls_merge),]
      spcfilter <- merge.data.frame(spcfilter, pls_merge)

      if("RMSE" %in% colnames(spcfilter)) spcfilter <- spcfilter[order(spcfilter$RMSE) , ]
      if("mad" %in% colnames(spcfilter)) spcfilter <- spcfilter[order(spcfilter$mad) , ]
    }
  }
  spcfilter <- spcfilter[!duplicated(spcfilter[ , 1:6]) , ]

  if(unique(unlist(lapply(spcfilter, length))) == 0) spcfilter <- NA
  return(spcfilter)
}
