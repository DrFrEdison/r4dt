
merge_pls <- function(pls_pred, pls_lm, mean=c(85,115), R2=.85, ordertype="sd"){
  merge_pls_lm_predict <- merge.data.frame(pls_pred$predict_parameter,pls_lm,by=intersect(names(pls_pred$predict_parameter),names(pls_lm)))

  if(length(is.na(merge_pls_lm_predict$mean>0))){
    merge_pls_lm_predict <- merge_pls_lm_predict[which(!is.na(merge_pls_lm_predict$mean)),]
  }

  if(length(is.na(merge_pls_lm_predict$RMSE>0))){
    merge_pls_lm_predict <- merge_pls_lm_predict[which(!is.na(merge_pls_lm_predict$RMSE)),]
  }

  merge_pls_lm_predict <- merge_pls_lm_predict[which(merge_pls_lm_predict$mean>mean[1] & merge_pls_lm_predict$mean<mean[2]),]
  # if(length(which(merge_pls_lm_predict$R2>R2)) == 0) stop("R2 lower as threshold R2")
  merge_pls_lm_predict <- merge_pls_lm_predict[which(merge_pls_lm_predict$R2>R2),]

  if(ordertype=="sd") merge_pls_lm_predict <- merge_pls_lm_predict[order(merge_pls_lm_predict$sd),]
  if(ordertype=="mad") merge_pls_lm_predict <- merge_pls_lm_predict[order(merge_pls_lm_predict$mad),]
  if(ordertype=="RMSE") merge_pls_lm_predict <- merge_pls_lm_predict[order(merge_pls_lm_predict$RMSE),]
  if(ordertype=="R2") merge_pls_lm_predict <- merge_pls_lm_predict[order(merge_pls_lm_predict$R2, decreasing = T),]

  return(merge_pls_lm_predict)
}
