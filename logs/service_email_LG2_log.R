# Create log file ####
require(this.path)
setwd(this.dir())
nodename <- as.character(gsub("[0-9.-]", "", Sys.info()["nodename"]))
setwd(this.dir())
setwd("..")
file.copy(dir(pattern = ".log$")
          , paste0(getwd(), "/logs/", substr(gsub("-","", as.Date(Sys.Date())), 3, 8), "_", gsub(".log", "", dir(pattern = ".log$")), "_", nodename, ".log"))
file.remove(dir(pattern = ".log$"))
