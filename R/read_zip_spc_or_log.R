read_log_files <- function( wd
                            , zipfile
                            , LG = "LG"
                            , log_or_spc = c("log", "spc")
                            , filetype = c("spc", "ref", "drk")
                            , delete_extract = T){
  
  zipinfo <- list()
  
  zipinfo$wd <- wd
  
  zipinfo$file  <- zipfile
  LG <- LG
  
  zipinfo$inside_file <- unzip(zipfile = zipinfo$file, list = TRUE)
  
  if( log_or_spc == "log"){
    zipinfo$inside_mod <- grep( ".log$", zipinfo$inside_file$Name, value = T)
    
    if( ! "spc" %in% filetype ) zipinfo$inside_mod <- grep("-M.log", zipinfo$inside_mod, value = T, invert = T)
    if( ! "ref" %in% filetype ) zipinfo$inside_mod <- grep("-R.log", zipinfo$inside_mod, value = T, invert = T)
    if( ! "drk" %in% filetype ) zipinfo$inside_mod <- grep("-D.log", zipinfo$inside_mod, value = T, invert = T)
    
  }
  
  if( log_or_spc == "spc"){
    zipinfo$inside_mod <- grep( "spc$", zipinfo$inside_file$Name, value = T)
    
    if( ! "spc" %in% filetype ) zipinfo$inside_mod <- grep("-M.spc", zipinfo$inside_mod, value = T, invert = T)
    if( ! "ref" %in% filetype ) zipinfo$inside_mod <- grep("-R.spc", zipinfo$inside_mod, value = T, invert = T)
    if( ! "drk" %in% filetype ) zipinfo$inside_mod <- grep("-D.spc", zipinfo$inside_mod, value = T, invert = T)
    
  }
  
  if( LG == "SG") untar(  zipfile = paste0( getwd(), "/", zipinfo$file)
                          , files = zipinfo$inside_mod)
  
  if( LG != "SG") unzip(  zipfile = paste0( getwd(), "/", zipinfo$file)
                          , files = zipinfo$inside_mod, overwrite = T)
  
  zipinfo$zipdir <- gsub("\\.", "", gsub(".7z", "", gsub("zip", "", zipinfo$file), zipinfo$file))
  setwd( zipinfo$zipdir )
  
  SG <- ifelse(LG == "SG", T, F)
  if( log_or_spc == "spc") drop_spc( getwd(), T)
  if( log_or_spc == "log" & "spc" %in% filetype) r4dt::drop_log( path = getwd(), recursive = T, type = "spc", SG = SG)
  if( log_or_spc == "log" & "ref" %in% filetype) r4dt::drop_log( path = getwd(), recursive = T, type = "ref", SG = SG)
  if( log_or_spc == "log" & "drk" %in% filetype) r4dt::drop_log( path = getwd(), recursive = T, type = "drk", SG = SG)
  
  file.copy( dir( pattern = ".csv" ), paste0(zipinfo$wd, "/", dir( pattern = ".csv" )))
  
  setwd( zipinfo$wd )
  
  if(delete_extract){
    unlink(zipinfo$zipdir, recursive = TRUE)
  }
}