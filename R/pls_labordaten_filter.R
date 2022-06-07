labordaten_lm_filter <- function(prediction, lm_compare, ncomp, lab_data_substance, lab_data_vec, quantile_c = 12){
  
  diff_lab_pred_spc <- matrix(nrow = length(names(prediction$spc)), ncol = ncomp)
  for(i in 1:nrow(diff_lab_pred_spc)){
    for(j in 1:ncomp){
      diff_lab_pred_spc[i,j] <- 
        mean(abs(prediction$spc[[i]][lab_data_vec,j] 
                 - lab_data_substance))
    }
  }
  
  diff_lab_pred_spc1st <- matrix(nrow = length(names(prediction$`1st`)), ncol = ncomp)
  for(i in 1:nrow(diff_lab_pred_spc1st)){
    for(j in 1:ncomp){
      diff_lab_pred_spc1st[i,j] <- 
        mean(abs(prediction$`1st`[[i]][lab_data_vec,j] 
                 - lab_data_substance))
    }
  }
  
  diff_lab_pred_spc2nd <- matrix(nrow = length(names(prediction$`2nd`)), ncol = ncomp)
  for(i in 1:nrow(diff_lab_pred_spc2nd)){
    for(j in 1:ncomp){
      diff_lab_pred_spc2nd[i,j] <- 
        mean(abs(prediction$`2nd`[[i]][lab_data_vec,j] 
                 - lab_data_substance))
    }
  }
  
  rownames(diff_lab_pred_spc) <- names(prediction$spc)
  rownames(diff_lab_pred_spc1st) <- names(prediction$`1st`)
  rownames(diff_lab_pred_spc2nd) <- names(prediction$`2nd`)
  
  if(quantile_c == 3){
    spc_q <- quantile(diff_lab_pred_spc)[3]
    spc_q1st <- quantile(diff_lab_pred_spc)[3]
    spc_q2nd <- quantile(diff_lab_pred_spc)[3]
  }
  
  if(quantile_c == 2){
    spc_q <- quantile(diff_lab_pred_spc)[2]
    spc_q1st <- quantile(diff_lab_pred_spc)[2]
    spc_q2nd <- quantile(diff_lab_pred_spc)[2]
  }
  
  if(quantile_c == 12){
    spc_q <- mean(quantile(diff_lab_pred_spc)[1:2])
    spc_q1st <- mean(quantile(diff_lab_pred_spc)[1:2])
    spc_q2nd <- mean(quantile(diff_lab_pred_spc)[1:2])
  }
  
  for(j in 1:ncomp){
    diff_lab_pred_spc[which(diff_lab_pred_spc[,j] >= spc_q),j] <- NA
    diff_lab_pred_spc1st[which(diff_lab_pred_spc1st[,j] >= spc_q1st),j] <- NA
    diff_lab_pred_spc2nd[which(diff_lab_pred_spc2nd[,j] >= spc_q2nd),j] <- NA
  }
  
  dropspc <- c()
  dropspc1st <- c()
  dropspc2nd <- c()
  
  for(i in 1:nrow(diff_lab_pred_spc)) if(all(is.na(diff_lab_pred_spc[i,]))) dropspc[i] <- i; diff_lab_pred_spc <- diff_lab_pred_spc[which(is.na(dropspc)), ]
  for(i in 1:nrow(diff_lab_pred_spc1st)) if(all(is.na(diff_lab_pred_spc1st[i,]))) dropspc1st[i] <- i; diff_lab_pred_spc1st <- diff_lab_pred_spc1st[which(is.na(dropspc1st)), ]
  for(i in 1:nrow(diff_lab_pred_spc2nd)) if(all(is.na(diff_lab_pred_spc2nd[i,]))) dropspc2nd[i] <- i; diff_lab_pred_spc2nd <- diff_lab_pred_spc2nd[which(is.na(dropspc2nd)), ]
  
  spcfilter <- matrix(nrow=nrow(diff_lab_pred_spc), ncol=ncomp)
  for(i in 1:nrow(spcfilter)){
    for(j in 1:ncomp){
      if(!is.na(diff_lab_pred_spc[i,j])){
        
        wl1 <- substr(rownames(diff_lab_pred_spc)[i],1,3)
        wl2 <- substr(rownames(diff_lab_pred_spc)[i],5,7)
        
        if(length(which(lm_compare$wl1 == wl1
                        & lm_compare$wl2 == wl2
                        & lm_compare$ncomp == j
                        & lm_compare$spc == "spc")) == 0){
          spcfilter[i,j] <- NA
        } else {
          spcfilter[i,j] <- which(lm_compare$wl1 == wl1
                                  & lm_compare$wl2 == wl2
                                  & lm_compare$ncomp == j
                                  & lm_compare$spc == "spc")  
        }
        
      } else spcfilter[i,j] <- NA
    }
  }
  
  spc1stfilter <- matrix(nrow=nrow(diff_lab_pred_spc1st), ncol=ncomp)
  for(i in 1:nrow(spc1stfilter)){
    for(j in 1:ncomp){
      if(!is.na(diff_lab_pred_spc1st[i,j])){
        
        wl1 <- substr(rownames(diff_lab_pred_spc1st)[i],1,3)
        wl2 <- substr(rownames(diff_lab_pred_spc1st)[i],5,7)
        
        if(length(which(lm_compare$wl1 == wl1
                        & lm_compare$wl2 == wl2
                        & lm_compare$ncomp == j
                        & lm_compare$spc == "1st")) == 0){
          spc1stfilter[i,j] <- NA
        } else {
          spc1stfilter[i,j] <- which(lm_compare$wl1 == wl1
                                     & lm_compare$wl2 == wl2
                                     & lm_compare$ncomp == j
                                     & lm_compare$spc == "1st")  
        }
        
      } else spc1stfilter[i,j] <- NA
    }
  }
  
  spc2ndfilter <- matrix(nrow=nrow(diff_lab_pred_spc2nd), ncol=ncomp)
  for(i in 1:nrow(spc2ndfilter)){
    for(j in 1:ncomp){
      if(!is.na(diff_lab_pred_spc2nd[i,j])){
        
        wl1 <- substr(rownames(diff_lab_pred_spc2nd)[i],1,3)
        wl2 <- substr(rownames(diff_lab_pred_spc2nd)[i],5,7)
        
        if(length(which(lm_compare$wl1 == wl1
                        & lm_compare$wl2 == wl2
                        & lm_compare$ncomp == j
                        & lm_compare$spc == "2nd")) == 0){
          spc2ndfilter[i,j] <- NA
        } else {
          spc2ndfilter[i,j] <- which(lm_compare$wl1 == wl1
                                     & lm_compare$wl2 == wl2
                                     & lm_compare$ncomp == j
                                     & lm_compare$spc == "2nd")  
        }
        
      } else spc2ndfilter[i,j] <- NA
    }
  }
  
  filtered_lm <- rbind(lm_compare[sort(unlist(spcfilter)),]
                       , lm_compare[sort(unlist(spc1stfilter)),]
                       , lm_compare[sort(unlist(spc2ndfilter)),]
  )
  
  filtered_lm <- filtered_lm[order(filtered_lm$sd),]
  return(filtered_lm)
}
