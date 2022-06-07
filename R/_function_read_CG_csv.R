read_txt_files_SG <- function(wd = getwd(), filename = paste0(substr(gsub("-","",Sys.Date()),3,8), "_CG_export"), recursive = T){
  
  setwd(wd)
  
  cg <- list()
  cg$filename <- filename
  
  cg$files <- dir(pattern = "txt$", recursive = recursive)
  cg$files <- cg$files[order(file.info(cg$files)$ctime)]
  
  suppressWarnings(cg$raw <- lapply(cg$files, function(x) fread(x,header=F,sep=";",dec = ".",encoding = "Latin-1")))
  
  if(length(which(unlist(lapply(cg$raw, ncol)) < 100)) > 0) message(paste("Datei", cg$files[which(unlist(lapply(cg$raw, ncol)) < 100)], "wird nicht mit exportiert"))
  if(length(which(unlist(lapply(cg$raw, ncol)) < 100)) > 0) cg$files <- cg$files[-which(unlist(lapply(cg$raw, ncol)) < 100)]
  if(length(which(unlist(lapply(cg$raw, ncol)) < 100)) > 0) cg$raw <- cg$raw[-which(unlist(lapply(cg$raw, ncol)) < 100)]
  
  cg$filesb <- basename(cg$files)
  
  suppressWarnings(cg$raw <- lapply(cg$raw, function(x) gsub("\\{", "", x)))
  suppressWarnings(cg$raw <- lapply(cg$raw, function(x) gsub("\\}", "", x)))
  suppressWarnings(cg$raw <- lapply(cg$raw, function(x) as.numeric(as.character(gsub(",", ".", x)))))
  
  if(length(unique(unlist(lapply(cg$raw[grep("_WL", cg$files)], length)))) != 1) stop(message("Da stimmt was nicht mit la-la-la-la-lambda"))
  
  cg$export$wl <- unique(unlist(cg$raw[grep("_WL", cg$files)]))
  
  cg$export$ref <- cg$raw[grep("_BGD_", cg$files)]
  cg$export$drk <- cg$raw[grep("_DRK_", cg$files)]
  cg$export$abs <- cg$raw[grep("AbsSpectrum", cg$files)]
  cg$export$mea <- cg$raw[grep("_MEA_", cg$files)]
  
  cg$export_finish <- rbind(cg$export$wl
                            , do.call(rbind, cg$export$abs)
                            , do.call(rbind, cg$export$ref)
                            , do.call(rbind, cg$export$drk)
                            , do.call(rbind, cg$export$mea))
  
  cg$export_finish <- cbind(c("FilePath", cg$files[-grep("_WL", cg$files)])
                            , c("FileName", cg$filesb[-grep("_WL", cg$filesb)])
                            , c("FileCreationDateTime", as.character(file.info(cg$files[-grep("_WL", cg$files)])$ctime))
                            , c("Type"
                                , rep("ABS", length(cg$export$abs))
                                , rep("REF", length(cg$export$ref))
                                , rep("DRK", length(cg$export$drk))
                                , rep("MEA", length(cg$export$mea))
                            )
                            , c("unit"
                                , rep("AU", length(cg$export$abs))
                                , rep("Counts", length(cg$export$ref))
                                , rep("Counts", length(cg$export$drk))
                                , rep("Counts", length(cg$export$mea))
                            )
                            , cg$export_finish)
  
  colnames(cg$export_finish) <- cg$export_finish[1,]
  cg$export_finish <- cg$export_finish[-1,]
  write.csv2(cg$export_finish, paste0(cg$filename, ".csv"), row.names = F)
  return(cg$export_finish)
}

