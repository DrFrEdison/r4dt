# return AU ####
spc_au <- function(spcdata_data_au,lambda=273,use_baseline=F){
  absorbance_at_nm_final <- list()
  ifelse(use_baseline==T,spcdata_data_au$spc_to_choose <- spcdata_data_au$spc_baseline, spcdata_data_au$spc_to_choose <- spcdata_data_au$spc)
  
  for(j in 1:length(lambda)){
    absorbance_at_nm <- mapply(function(x,y) y[which(x==lambda[j])],
                               spcdata_data_au$wl,
                               spcdata_data_au$spc_to_choose,SIMPLIFY = F)
    
    filestext <- list()
    for(i in 1:length(spcdata_data_au$files)) filestext[[i]] <- rep(spcdata_data_au$filestext[i],length(absorbance_at_nm[[i]]))
    
    absorbance_at_nm_final[[j]] <- data.frame(files=unlist(filestext),nm=lambda[j],au=unlist(absorbance_at_nm))
  }
  
  absorbance_at_nm_final <- data.frame(do.call(rbind,absorbance_at_nm_final))
  absorbance_at_nm_final$files <- factor(absorbance_at_nm_final$files)
  absorbance_at_nm_final$nm <- factor(absorbance_at_nm_final$nm)
  return(absorbance_at_nm_final)
}