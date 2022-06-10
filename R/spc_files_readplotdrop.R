drop_spc <- function(path = dirname(rstudioapi::getSourceEditorContext()$path), recursive = T){
  writespc <- list()

  writespc$export <- c("ref", "au", "trans", "drk")
  writespc$spc_daten <-  read_spc_files(directory = path
                                        , baseline = writespc$baseline <- NA
                                        , pngplot = F
                                        , plotlyplot = F
                                        , recursive = recursive
                                        , filestext = NA
                                        , colp = NA
                                        , subfiles = NA)

  setwd(path)
  for(i in 1:length(writespc$export)){
    if(length(writespc$spc_daten[[grep(writespc$export[i], names(writespc$spc_daten))]]) == 0) writespc$export[i] <- NA
    if(is.na(writespc$export[i])) next

    namep <- paste0(.date(), "_", writespc$export[i])
    if(all(writespc$export[i] == "au", !is.na(writespc$baseline))) namep <- paste0(date(), "_", writespc$export[i], "_bl")

    write_spc_files(spc_file = writespc$spc_daten[[grep(writespc$export[i], names(writespc$spc_daten))]]
                    , baseline = all(writespc$export[i] == "au", !is.na(writespc$baseline))
                    , write = T
                    , filename = namep
                    , return_R = F)
  }
}
