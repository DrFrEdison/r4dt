merge_pls_site <- function(merge_pls_lm_predict_ls, number=20, ncomp=NA){

  if(!is.na(ncomp)) for(i in 1:length(merge_pls_lm_predict_ls)) merge_pls_lm_predict_ls[[i]] <- merge_pls_lm_predict_ls[[i]][which(merge_pls_lm_predict_ls[[i]][,"ncomp"]<=ncomp),]

  for(i in 1:length(merge_pls_lm_predict_ls)) merge_pls_lm_predict_ls[[i]] <- merge_pls_lm_predict_ls[[i]][which(!duplicated.data.frame(merge_pls_lm_predict_ls[[i]])),]

  tocompare <- list()

  for(i in 1:length(merge_pls_lm_predict_ls)){
    if("wl3" %in% names(merge_pls_lm_predict_ls[[1]])) tocompare[[i]] <- merge_pls_lm_predict_ls[[i]][1:number,c("wl1","wl2","wl3","wl4","ncomp","spc")]
    if(!"wl3" %in% names(merge_pls_lm_predict_ls[[1]])) tocompare[[i]] <- merge_pls_lm_predict_ls[[i]][1:number,c("wl1","wl2","ncomp","spc")]
  }

  if("wl3" %in% names(merge_pls_lm_predict_ls[[1]])) tocompare <- lapply(tocompare,function(x) paste0(x$wl1,x$wl2,x$wl3,x$wl4,x$ncomp,x$spc))
  if(!"wl3" %in% names(merge_pls_lm_predict_ls[[1]])) tocompare <- lapply(tocompare,function(x) paste0(x$wl1,x$wl2,x$ncomp,x$spc))

  tocomparedup <- data.frame(matrix(nrow=length(tocompare[[1]]), ncol=(length(tocompare))))
  for(j in 1:length(tocompare)) for(i in 1:length(tocompare[[1]])) tocomparedup[i,j] <- tocompare[[1]][i] %in% tocompare[[j]]
  for(i in 1:nrow(tocomparedup)) tocomparedup[i,ncol(tocomparedup)] <- all(t(apply(tocomparedup,1,function(x) as.character(x)==T))[i,])

  if("wl3" %in% names(merge_pls_lm_predict_ls[[1]])) returnlist <- merge_pls_lm_predict_ls[[1]][1:number,c("spc", "wl1","wl2","wl3","wl4","ncomp","mean","sd","median","mad","R2","RMSE")][which(tocomparedup[,ncol(tocomparedup)]==T),]
  if(!"wl3" %in% names(merge_pls_lm_predict_ls[[1]])) returnlist <- merge_pls_lm_predict_ls[[1]][1:number,c("spc", "wl1","wl2","ncomp","mean","sd","median","mad","R2","RMSE")][which(tocomparedup[,ncol(tocomparedup)]==T),]
  returnlist$mean <- round(returnlist$mean,1)
  returnlist$sd <- round(returnlist$sd,2)
  returnlist$median <- round(returnlist$median,1)
  returnlist$mad <- round(returnlist$mad,2)
  returnlist$R2 <- round(returnlist$R2,2)
  returnlist$RMSE <- round(returnlist$RMSE,2)
  if(length(grep("NA", rownames(returnlist))) > 0) returnlist <- returnlist[-grep("NA", rownames(returnlist)) , ]
  return(returnlist)
}
