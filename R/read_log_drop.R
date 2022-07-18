drop_log <- function(path = dirname(rstudioapi::getSourceEditorContext()$path), recursive = T, type = "spc", SG = F){
  writelog <- list()

  if( type == "spc")  writelog$spc <- dir( path = path, pattern = "M.log")
  if( type == "ref")  writelog$spc <- dir( path = path, pattern = "R.log")
  if( type == "drk")  writelog$spc <- dir( path = path, pattern = "D.log")

  setwd(path)

  read_log_files(logfiles = writelog$spc, SG = SG, type = type)

}
