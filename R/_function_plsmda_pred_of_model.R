pred_of_model_mda <- function(prediction, wl1, wl2, wl3, wl4, ncomp, derivative){
  
  
  final_prediction <- as.numeric(prediction$prediction[[ grep(derivative, names(prediction$prediction))[1] ]] [[ncomp]][[1]]$y.pred[,ncomp,])
  
  return(final_prediction)
}
