pred_of_model <- function(prediction, wl1, wl2, wl3, wl4, ncomp, derivative){
  
  final_prediction <- as.numeric(prediction$prediction[[1]][[ncomp]])
  
  return(final_prediction)
}
