drop_log <- function(path = dirname(rstudioapi::getSourceEditorContext()$path), recursive = T){
  writelog <- list()
  
  writelog$spc <- dir( path = path, pattern = "M.log")
  writelog$ref <- dir( path = path, pattern = "R.log")
  writelog$drk <- dir( path = path, pattern = "D.log")
  
  setwd(path)
  
  writelog$read_spc <-  read_log_files(logfiles = writelog$spc
                                       , SG = SG)
}
