spectra.validation.drk <- function(spc, c1 = 9, c2 = 90){
  spc.val.drk <- list()
  spc.val.drk$mad <- mad(as.numeric(spc))
  if(!is.na(spc.val.drk$mad)){
    if(spc.val.drk$mad < c1) spc.val.drk$val = "valid"
    if(spc.val.drk$mad >= c1 & spc.val.drk$mad <= c2) spc.val.drk$val = "critical"
    if(spc.val.drk$mad > c2) spc.val.drk$val = "invalid"}
  if(is.na(spc.val.drk$mad)) spc.val.drk$val = "empty"
  
  spc.val.drk$val <- factor(spc.val.drk$val, levels = c("valid", "critical", "invalid", "empty"))
  
  return(spc.val.drk$val)
}