produktion_prediction <- function(csv_transfered,pls_function_obj,ncomp,messaget = T) {
  options(warn = -1)
  
  if("spc" %in% names(pls_function_obj)) predict_plsmodel_df <- data.frame()
  if("spc1st" %in% names(pls_function_obj)) predict_plsmodel_1st_df <- data.frame()
  if("spc2nd" %in% names(pls_function_obj)) predict_plsmodel_2nd_df <- data.frame()
  
  if("spc" %in% names(pls_function_obj)) predict_plsmodel <- list()
  if("spc1st" %in% names(pls_function_obj)) predict_plsmodel_1st <- list()
  if("spc2nd" %in% names(pls_function_obj)) predict_plsmodel_2nd <- list()
  
  if("spc" %in% names(pls_function_obj)) if(is.null(colnames(csv_transfered$spc))) csv_transfered$spc <- t(csv_transfered$spc)
  if("spc1st" %in% names(pls_function_obj)) if(is.null(colnames(csv_transfered$spc))) csv_transfered$spc1st <- t(csv_transfered$spc1st)
  if("spc2nd" %in% names(pls_function_obj)) if(is.null(colnames(csv_transfered$spc))) csv_transfered$spc2nd <- t(csv_transfered$spc2nd)
  
  spcpos <- grep("spc", names(pls_function_obj))
  spcpos1 <- spcpos[1]
  
  wlr <- data.frame(wl1 = substr(names(pls_function_obj[[spcpos1]]), 1, 3)
                    , wl2 = substr(names(pls_function_obj[[spcpos1]]), 5, 7)
                    , wl3 = substr(names(pls_function_obj[[spcpos1]]), 9, 11)
                    , wl4 = substr(names(pls_function_obj[[spcpos1]]), 13, 15))
  wlr$wl3[which(wlr$wl3 == "")] <- NA
  wlr$wl4[which(wlr$wl4 == "")] <- NA
  
  wlrl <- apply(wlr, 1, function(x) c(x[1]:x[2], if(!is.na(x[3])) x[3]:x[4]))
  ifelse(nrow(wlr) > 1
         , wlrlp <- lapply(wlrl, function(x) which(csv_transfered$wl %in% x))
         , wlrlp <- which(csv_transfered$wl %in% wlrl))
  if(nrow(wlr) == 1) wlrlp <- list(wlrlp)
  #wlrlp <- lapply(wlrl, function(x) which(csv_transfered$wl %in% x))
 
  predict_plsmodel_df <- list()
  predict_plsmodel_1st_df  <- list()
  predict_plsmodel_2nd_df  <- list()
  
  for(j in 1:ncomp){
    
    if("spc" %in% names(pls_function_obj)){
      predict_plsmodel[[j]] <- mapply(function(x, z) predict(object = x
                                                             , newdata = as.matrix(csv_transfered$spc[ , z])
                                                             , ncomp = j
                                                             , se.fit = T, interval = "confidence", level = 0.95)
                                      , x = pls_function_obj$spc
                                      , z = wlrlp)
      
      predict_plsmodel_df[[j]] <- data.frame(wlr
                                             , ncomp = j
                                             , mean = unlist(apply(predict_plsmodel[[j]], 2, function(y) mean(y, na.rm = T)))
                                             , sd = unlist(apply(predict_plsmodel[[j]], 2, function(y) sd(y, na.rm = T)))
                                             , median = unlist(apply(predict_plsmodel[[j]], 2, function(y) median(y, na.rm = T)))
                                             , mad = unlist(apply(predict_plsmodel[[j]], 2, function(y) mad(y, na.rm = T)))
                                             , min = unlist(apply(predict_plsmodel[[j]], 2, function(y) min(y, na.rm = T)))
                                             , max = unlist(apply(predict_plsmodel[[j]], 2, function(y) max(y, na.rm = T))))
      if(messaget == T) message(paste0("Prediction of PC", j, " for spc finished"))
      }
    
    if("spc1st" %in% names(pls_function_obj)){
      predict_plsmodel_1st[[j]] <- mapply(function(x, z) predict(object = x
                                                                 , newdata = as.matrix(csv_transfered$spc1st[ , z])
                                                                 , ncomp = j
                                                                 , se.fit = T, interval = "confidence", level = 0.95)
                                          , x = pls_function_obj$spc1st
                                          , z = wlrlp)
      
      predict_plsmodel_1st_df[[j]] <- data.frame(wlr
                                                 , ncomp = j
                                                 , mean = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) mean(y, na.rm = T)))
                                                 , sd = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) sd(y, na.rm = T)))
                                                 , median = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) median(y, na.rm = T)))
                                                 , mad = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) mad(y, na.rm = T)))
                                                 , min = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) min(y, na.rm = T)))
                                                 , max = unlist(apply(predict_plsmodel_1st[[j]], 2, function(y) max(y, na.rm = T))))
      if(messaget == T) message(paste0("Prediction of PC", j, " for 1st derivative finished"))
      }
    
    if("spc2nd" %in% names(pls_function_obj)){
      predict_plsmodel_2nd[[j]] <- mapply(function(x, z) predict(object = x
                                                                 , newdata = as.matrix(csv_transfered$spc2nd[ , z])
                                                                 , ncomp = j
                                                                 , se.fit = T, interval = "confidence", level = 0.95)
                                          , x = pls_function_obj$spc2nd
                                          , z = wlrlp)
      
      predict_plsmodel_2nd_df[[j]] <- data.frame(wlr
                                                 , ncomp = j
                                                 , mean = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) mean(y, na.rm = T)))
                                                 , sd = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) sd(y, na.rm = T)))
                                                 , median = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) median(y, na.rm = T)))
                                                 , mad = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) mad(y, na.rm = T)))
                                                 , min = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) min(y, na.rm = T)))
                                                 , max = unlist(apply(predict_plsmodel_2nd[[j]], 2, function(y) max(y, na.rm = T))))
      if(messaget == T)  message(paste0("Prediction of PC", j, " for 2nd derivative finished"))
    }
    if(messaget == T)  if(j != ncomp) message(paste0("Prediction of PC", j, " finished, ", ncomp - j, " to go"))
    if(messaget == T) if(j == ncomp) message(paste0("Prediction finished"))
  }
  
  if("spc" %in% names(pls_function_obj)) predict_plsmodel_df <- do.call(rbind, predict_plsmodel_df)
  if("spc1st" %in% names(pls_function_obj)) predict_plsmodel_1st_df <- do.call(rbind, predict_plsmodel_1st_df)
  if("spc2nd" %in% names(pls_function_obj)) predict_plsmodel_2nd_df <- do.call(rbind, predict_plsmodel_2nd_df)
  
  predict_plsmodel_df <- rbind(if("spc" %in% names(pls_function_obj)) cbind(spc="spc",predict_plsmodel_df),
                               if("spc1st" %in% names(pls_function_obj)) cbind(spc="1st",predict_plsmodel_1st_df),
                               if("spc2nd" %in% names(pls_function_obj)) cbind(spc="2nd",predict_plsmodel_2nd_df)
  )
  predict_plsmodel_df <- predict_plsmodel_df[order(predict_plsmodel_df$mad), ]
  rownames(predict_plsmodel_df) <- 1:nrow(predict_plsmodel_df)
  
  prediction_all <- list(if("spc" %in% names(pls_function_obj)) predict_plsmodel,
                         if("spc1st" %in% names(pls_function_obj)) predict_plsmodel_1st,
                         if("spc2nd" %in% names(pls_function_obj)) predict_plsmodel_2nd)
  
  if(!"spc2nd" %in% names(pls_function_obj)) prediction_all[[3]] <- NULL
  if(!"spc1st" %in% names(pls_function_obj)) prediction_all[[2]] <- NULL
  if(!"spc" %in% names(pls_function_obj)) prediction_all[[1]] <- NULL
  
  names(prediction_all) <- names(pls_function_obj)[spcpos]
  # prediction_all <- prediction_all[which(names(prediction_all) %in% names(pls_function_obj))]
  
  prediction_return <- list(predict_plsmodel_df,prediction_all)
  names(prediction_return) <- c("predict_parameter","prediction")
  return(prediction_return)
}
