produktion_prediction_lin <- function(csv_transfered,pls_function_obj) {
  options(warn = -1)
  
  # Create Empty List ####
  pls_pred <- list()
  
  csv_transfered$spc <- t(csv_transfered$spc)
  csv_transfered$spc1st <- t(csv_transfered$spc1st)
  csv_transfered$spc2nd <- t(csv_transfered$spc2nd)
  
  # Wavelength ####
  wlr <- data.frame(wl1 = substr(names(pls_function_obj), 1, 3)
                    , wl2 = substr(names(pls_function_obj), 5, 7)
                    , wl3 = substr(names(pls_function_obj), 9, 11)
                    , wl4 = substr(names(pls_function_obj), 13, 15))
  
  # check for non numeric and make them NA
  wlr <- apply(wlr, 2, function(x) as.numeric(gsub('\\D', '', x)))
  wlr[ , 3][which(nchar(wlr[,3]) < 2 | is.na(nchar(wlr[,3])))] <- NA
  wlr[ , 4][which(nchar(wlr[,4]) < 2 | is.na(nchar(wlr[,4])))] <- NA
  
  wlrl <- apply(wlr, 1, function(x) c(x[1]:x[2], if(!is.na(x[3])) x[3]:x[4]))
  
  if(!is.null(nrow(wlrl))){  if(nrow(wlrl) > nrow(wlr)) wlrl <- lapply(apply(wlrl, 2, list), unlist) }
  
  ifelse(nrow(wlr) > 1
         , wlrlp <- lapply(wlrl, function(x) which(csv_transfered$wl %in% x))
         , wlrlp <- which(csv_transfered$wl %in% wlrl))
  if(nrow(wlr) == 1) wlrlp <- list(wlrlp)
  
  # read ncomp from names
  ncomp <- as.numeric(gsub('\\D', "", substr(names(pls_function_obj), nchar(names(pls_function_obj)) - 1, nchar(names(pls_function_obj)))))
  
  # read spc from names
  spc <- gsub("_", "",  substr(names(pls_function_obj)
                               , unlist(lapply(gregexpr("_", names(pls_function_obj)), function(x) x[length(x) - 1]))
                               , unlist(lapply(gregexpr("_", names(pls_function_obj)), max))))
  
  ncompl <- as.list(ncomp)
  spcl <- as.list(spc)
  spcll <- lapply(spcl, function(x) grep(x, names(csv_transfered))[1])
  
  pls_pred <- mapply(function(x, z, y, w) predict(object = x
                                                         , newdata = t(as.matrix(csv_transfered[[w]][ z , ]))
                                                         , ncomp = y
                                                         , se.fit = T, interval = "confidence", level = 0.95)
                                  , x = pls_function_obj
                                  , z = wlrlp
                                  , y = ncompl
                                  , w = spcll
                                  , SIMPLIFY = F)
  
  pls_pred_sum <- data.frame(wlr
                             , spc = spc
                             , ncomp = ncomp
                             , mean = as.numeric(unlist(lapply(pls_pred, function(x) mean(x, na.rm = T))))
                             , sd = as.numeric(unlist(lapply(pls_pred, function(x) sd(x, na.rm = T))))
                             , median = as.numeric(unlist(lapply(pls_pred, function(x) median(x, na.rm = T))))
                             , mad = as.numeric(unlist(lapply(pls_pred, function(x) mad(x, na.rm = T))))
                             , max = as.numeric(unlist(lapply(pls_pred, function(x) max(x, na.rm = T))))
                             , min = as.numeric(unlist(lapply(pls_pred, function(x) min(x, na.rm = T))))
  )
  
  prediction_return <- list(pls_pred_sum, pls_pred)
  names(prediction_return) <- c("predict_parameter","prediction")
  return(prediction_return)
}
