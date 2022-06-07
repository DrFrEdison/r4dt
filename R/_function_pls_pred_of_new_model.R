pred_of_new_model <- function(modell_csv_transfered, substance, wl1, wl2, wl3, wl4, ncomp, derivative, csv_transfered, T2 = F){
  
  if(is.na(wl4)) wlr <- .wlr_function(wl1,wl2,1)
  if(!is.na(wl4)) wlr <- data.frame(Var1 = wl1, Var2 = wl2, Var3 = wl3, Var4 = wl4)
  
  pls_list <- list()
  pls_list$pls <- pls_function(csv_transfered = modell_csv_transfered, substance = substance, wlr = wlr,ncomp = ncomp, spc = derivative)
  pls_list$lm <- pls_lm_function(pls_list$pls, csv_transfered = modell_csv_transfered, substance = substance,wlr = wlr,ncomp = ncomp)
  
  if(T2 == T){
    h2 <- mdatools::pls(modell_csv_transfered[[grep(derivative, names(modell_csv_transfered))[1]]]
                        , modell_csv_transfered$data[ , grep(substance, names(modell_csv_transfered$data))]
                        , ncomp)
    
    h2pred <- predict(h2, csv_transfered[[grep(derivative, names(csv_transfered))[1]]], y = NULL, cv = F)
  }
  
  pls_list$pred <- list()
  pls_list$function_obj <- pls_list$pls
  pls_list$pred <- produktion_prediction(csv_transfered = csv_transfered, pls_function_obj = pls_list$function_obj, ncomp = ncomp)
  
  pls_list$model_pred <- pred_of_model(prediction = pls_list$pred, wl1, wl2, wl3, wl4, ncomp, derivative)
  
  if(T2 == F) return(pls_list$model_pred)
  if(T2 == T){
    exportlist <- list(pls_list$model_pred
                       , h2$T2lim[2, ncomp]
                       , h2pred$xdecomp$T2[ , ncomp])
    names(exportlist) <- c("model_pred", "T2lim", "T2")
  }
  return(exportlist)
}