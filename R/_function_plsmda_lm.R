plsmda_lm_function <- function(pls_function_obj,csv_transfered,substance,wlr,ncomp){
  
  pls_lm <- list()
  pls_lm$spc <- list()
  pls_lm$spc1st <- list()
  pls_lm$spc2nd <- list()
  pls_lm$plsRMSE <- list()
  pls_lm$pls_1stRMSE <- list()
  pls_lm$pls_2ndRMSE <- list()
  pls_lm$pls_slope <- list()
  pls_lm$pls_1stslope <- list()
  pls_lm$pls_2ndslope <- list()
  pls_lm$plsR2 <- list()
  pls_lm$pls_1stR2 <- list()
  pls_lm$pls_2ndR2 <- list()
  pls_lm$ncomp <- list()
  pls_lm$ncomp_1st <- list()
  pls_lm$ncomp_2nd <- list()
  
  pls_lm_summary <- data.frame()
  
  spcpos <- grep("spc", names(pls_function_obj))
  spcpos1 <- spcpos[1]
  
  wlr <- data.frame(wl1 = substr(names(pls_function_obj[[spcpos1]]), 1, 3)
                    , wl2 = substr(names(pls_function_obj[[spcpos1]]), 5, 7)
                    , wl3 = substr(names(pls_function_obj[[spcpos1]]), 9, 11)
                    , wl4 = substr(names(pls_function_obj[[spcpos1]]), 13, 15))
  
  wlr$wl3[which(wlr$wl3 == "")] <- NA
  wlr$wl4[which(wlr$wl4 == "")] <- NA
  
  for(j in 1:ncomp){
    if("spc" %in% names(pls_function_obj)){
      pls_lm$plsRMSE[[j]] <- lapply(pls_function_obj$spc, function(x) x$calres$rmse[,j])
      pls_lm$pls_slope[[j]] <- lapply(pls_function_obj$spc, function(x) x$calres$slope[,j])
      pls_lm$plsR2[[j]] <- lapply(pls_function_obj$spc, function(x) x$calres$r2[,j])
      pls_lm$ncomp[[j]] <- j
    }
    
    if("spc1st" %in% names(pls_function_obj)){
      pls_lm$pls_1stRMSE[[j]] <- lapply(pls_function_obj$spc1st, function(x) x$calres$rmse[,j])
      pls_lm$pls_1stslope[[j]] <- lapply(pls_function_obj$spc1st, function(x) x$calres$slope[,j])
      pls_lm$pls_1stR2[[j]] <- lapply(pls_function_obj$spc1st, function(x) x$calres$r2[,j])
      pls_lm$ncomp_1st[[j]] <- j
    }
    
    if("spc2nd" %in% names(pls_function_obj)){
      pls_lm$pls_2ndRMSE[[j]] <- lapply(pls_function_obj$spc2nd, function(x) x$calres$rmse[,j])
      pls_lm$pls_2ndslope[[j]] <- lapply(pls_function_obj$spc2nd, function(x) x$calres$slope[,j])
      pls_lm$pls_2ndR2[[j]] <- lapply(pls_function_obj$spc2nd, function(x) x$calres$r2[,j])
      pls_lm$ncomp_2nd[[j]] <- j
    }
  }
 
  if("spc" %in% names(pls_function_obj)) 
    pls_lm_summary <- cbind(ncomp = as.numeric(do.call(rbind, pls_lm$ncomp))
                               , wlr
                               , spc = "spc"
                               , Slope = as.numeric(do.call(rbind, pls_lm$pls_slope))
                               , RMSE = as.numeric(do.call(rbind, pls_lm$plsRMSE))
                               , R2 = as.numeric(do.call(rbind, pls_lm$plsR2)))

  if("spc1st" %in% names(pls_function_obj)) 
    pls_lm_summary1st <- cbind(ncomp = as.numeric(do.call(rbind, pls_lm$ncomp_1st))
                               , wlr
                               , spc = "1st"
                               , Slope = as.numeric(do.call(rbind, pls_lm$pls_1stslope))
                               , RMSE = as.numeric(do.call(rbind, pls_lm$pls_1stRMSE))
                               , R2 = as.numeric(do.call(rbind, pls_lm$pls_1stR2)))
  
  if("spc2nd" %in% names(pls_function_obj)) 
    pls_lm_summary2nd <- cbind(ncomp = as.numeric(do.call(rbind, pls_lm$ncomp_2nd))
        , wlr
        , spc = "2nd"
        , Slope = as.numeric(do.call(rbind, pls_lm$pls_2ndslope))
        , RMSE = as.numeric(do.call(rbind, pls_lm$pls_2ndRMSE))
        , R2 = as.numeric(do.call(rbind, pls_lm$pls_2ndR2)))
  
  pls_lm_summary <- rbind(if("spc" %in% names(pls_function_obj)) pls_lm_summary
                          , if("spc1st" %in% names(pls_function_obj)) pls_lm_summary1st
                          , if("spc2nd" %in% names(pls_function_obj)) pls_lm_summary2nd)
  
  rownames(pls_lm_summary) <- 1:nrow(pls_lm_summary)
  pls_lm_summary$slope <- round(pls_lm_summary$Slope,3)
  pls_lm_summary$R2 <- round(pls_lm_summary$R2,3)
  pls_lm_summary$RMSE <- round(pls_lm_summary$RMSE,2)
  
  return(pls_lm_summary)
}
