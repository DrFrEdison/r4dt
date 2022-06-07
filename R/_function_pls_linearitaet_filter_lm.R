linearitaet_lm_filter <- function(lin_filter, prediction_lm, ncomp){
  
  if(!"wl3" %in% names(prediction_lm)){
    prediction_lm <- cbind(prediction_lm, wl3 = NA, wl4 = NA)
  }
  
  if(length(lin_filter$spc) > 0){
    spcfilter <- matrix(nrow=length(lin_filter$spc), ncol=ncomp)
    for(i in 1:nrow(lin_filter$spc)){
      for(j in 1:ncomp){
        if(!is.na(lin_filter$spc[i,j])){
          
          wl1 <- substr(rownames(lin_filter$spc)[i],1,3)
          wl2 <- substr(rownames(lin_filter$spc)[i],5,7)
          wl3 <- substr(rownames(lin_filter$spc)[i],9,11)
          wl4 <- substr(rownames(lin_filter$spc)[i],13,15)
          
          if(is.na(as.numeric(wl4))){
            if(length(which(prediction_lm$wl1 == wl1
                            & prediction_lm$wl2 == wl2
                            & is.na(prediction_lm$wl3)
                            & is.na(prediction_lm$wl4)
                            & prediction_lm$ncomp == j
                            & prediction_lm$spc == "spc")) == 0){
              spcfilter[i,j] <- NA
            } else {
              spcfilter[i,j] <- which(prediction_lm$wl1 == wl1
                                      & prediction_lm$wl2 == wl2
                                      & is.na(prediction_lm$wl3)
                                      & is.na(prediction_lm$wl4)
                                      & prediction_lm$ncomp == j
                                      & prediction_lm$spc == "spc")  
            }
          }
          
          if(!is.na(as.numeric(wl4))){
            if(length(which(prediction_lm$wl1 == wl1
                            & prediction_lm$wl2 == wl2
                            & prediction_lm$wl3 == wl3
                            & prediction_lm$wl4 == wl4
                            & prediction_lm$ncomp == j
                            & prediction_lm$spc == "spc")) == 0){
              spcfilter[i,j] <- NA
            } else {
              spcfilter[i,j] <- which(prediction_lm$wl1 == wl1
                                      & prediction_lm$wl2 == wl2
                                      & prediction_lm$wl3 == wl3
                                      & prediction_lm$wl4 == wl4
                                      & prediction_lm$ncomp == j
                                      & prediction_lm$spc == "spc")  
            }
          }
          
        } else spcfilter[i,j] <- NA
      }
    }
  }
  
  if(length(lin_filter$`1st`) > 0){
    spc1stfilter <- matrix(nrow=length(lin_filter$`1st`), ncol=ncomp)
    for(i in 1:nrow(lin_filter$`1st`)){
      for(j in 1:ncomp){
        if(!is.na(lin_filter$`1st`[i,j])){
          
          wl1 <- substr(rownames(lin_filter$`1st`)[i],1,3)
          wl2 <- substr(rownames(lin_filter$`1st`)[i],5,7)
          wl3 <- substr(rownames(lin_filter$`1st`)[i],9,11)
          wl4 <- substr(rownames(lin_filter$`1st`)[i],13,15)
          
          if(is.na(as.numeric(wl4))){
            if(length(which(prediction_lm$wl1 == wl1
                            & prediction_lm$wl2 == wl2
                            & is.na(prediction_lm$wl3)
                            & is.na(prediction_lm$wl4)
                            & prediction_lm$ncomp == j
                            & prediction_lm$spc == "1st")) == 0){
              spc1stfilter[i,j] <- NA
            } else {
              spc1stfilter[i,j] <- which(prediction_lm$wl1 == wl1
                                         & prediction_lm$wl2 == wl2
                                         & is.na(prediction_lm$wl3)
                                         & is.na(prediction_lm$wl4)
                                         & prediction_lm$ncomp == j
                                         & prediction_lm$spc == "1st")  
            }
          }
          
          if(!is.na(as.numeric(wl4))){
            if(length(which(prediction_lm$wl1 == wl1
                            & prediction_lm$wl2 == wl2
                            & prediction_lm$wl3 == wl3
                            & prediction_lm$wl4 == wl4
                            & prediction_lm$ncomp == j
                            & prediction_lm$spc == "1st")) == 0){
              spc1stfilter[i,j] <- NA
            } else {
              spc1stfilter[i,j] <- which(prediction_lm$wl1 == wl1
                                         & prediction_lm$wl2 == wl2
                                         & prediction_lm$wl3 == wl3
                                         & prediction_lm$wl4 == wl4
                                         & prediction_lm$ncomp == j
                                         & prediction_lm$spc == "1st")  
            }
          }
          
        } else spc1stfilter[i,j] <- NA
      }
    }
  }
  
  if(length(lin_filter$`2nd`) > 0){
    spc2ndfilter <- matrix(nrow=length(lin_filter$`2nd`), ncol=ncomp)
    for(i in 1:nrow(lin_filter$`2nd`)){
      for(j in 1:ncomp){
        if(!is.na(lin_filter$`2nd`[i,j])){
          
          wl1 <- substr(rownames(lin_filter$`2nd`)[i],1,3)
          wl2 <- substr(rownames(lin_filter$`2nd`)[i],5,7)
          wl3 <- substr(rownames(lin_filter$`2nd`)[i],9,11)
          wl4 <- substr(rownames(lin_filter$`2nd`)[i],13,15)
          
          if(is.na(as.numeric(wl4))){
            if(length(which(prediction_lm$wl1 == wl1
                            & prediction_lm$wl2 == wl2
                            & is.na(prediction_lm$wl3)
                            & is.na(prediction_lm$wl4)
                            & prediction_lm$ncomp == j
                            & prediction_lm$spc == "2nd")) == 0){
              spc2ndfilter[i,j] <- NA
            } else {
              spc2ndfilter[i,j] <- which(prediction_lm$wl1 == wl1
                                         & prediction_lm$wl2 == wl2
                                         & is.na(prediction_lm$wl3)
                                         & is.na(prediction_lm$wl4)
                                         & prediction_lm$ncomp == j
                                         & prediction_lm$spc == "2nd")  
            }
          }
          
          
          prediction_lm[which(prediction_lm$wl1 == wl1
                              & prediction_lm$wl2 == wl2
                              #prediction_lm$wl3
                              & prediction_lm$ncomp == j
                              & prediction_lm$spc == "2nd")  , ]
          
          if(!is.na(as.numeric(wl4))){
            if(length(which(prediction_lm$wl1 == wl1
                            & prediction_lm$wl2 == wl2
                            & prediction_lm$wl3 == wl3
                            & prediction_lm$wl4 == wl4
                            & prediction_lm$ncomp == j
                            & prediction_lm$spc == "2nd")) == 0){
              spc2ndfilter[i,j] <- NA
            } else {
              spc2ndfilter[i,j] <- which(prediction_lm$wl1 == wl1
                                         & prediction_lm$wl2 == wl2
                                         & prediction_lm$wl3 == wl3
                                         & prediction_lm$wl4 == wl4
                                         & prediction_lm$ncomp == j
                                         & prediction_lm$spc == "2nd")  
            }
          }
          
        } else spc2ndfilter[i,j] <- NA
      }
    }
  }
  
  
  filtered_lm <- rbind(if(length(lin_filter$spc) > 0){prediction_lm[sort(unlist(spcfilter)),]}
                       , if(length(lin_filter$`1st`) > 0){prediction_lm[sort(unlist(spc1stfilter)),]}
                       , if(length(lin_filter$`2nd`) > 0){prediction_lm[sort(unlist(spc2ndfilter)),]}
  )
  if(!is.null(filtered_lm)) filtered_lm <- filtered_lm[order(filtered_lm$mad, decreasing = F),]
  return(filtered_lm)
}
