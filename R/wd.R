options(warn=-1)

require(this.path)

# working directory
wd <- list()
# wd$here <- this.dir()
# setwd("..")
# wd$here <- getwd()

# commonly used paths in my working directory
# wd$scripts <- paste0(wd$here,"R/")
# wd$output <- paste0(wd$here,"output/")

# R user scripts
# wd$R.user <- paste0(substr(dt$R, 1, unlist(gregexpr("/", dt$R))[length(unlist(gregexpr("/", dt$R))) - 1]), "R/")
# wd$R.user.logs <- paste0(wd$R.user, "logs/")

# Entertain ####

# csvtemp und Desktop ####
suppressMessages(suppressWarnings(library(fs, quietly = T)))
suppressMessages(suppressWarnings(library(data.table, quietly = T)))
wd$csvtemp <- "C:/csvtemp"
wd$desktop <- paste0(path_home(), "/Desktop/")
# Model ####

# F&E ####
wd$fe <- paste0(Sys.getenv("OneDriveCommercial"), "/FE_Methoden/")

# data ####
wd$data <- paste0(wd$fe[1],"Allgemein/R/data")

# model ####
wd$model <- paste0(wd$data, "/model/")

wd$model$CCEP <- paste0(wd$model[1],"/CCEP/")
wd$model$MEG <- paste0(wd$model[1],"/MEG/")
wd$model$PepsiCo <- paste0(wd$model[1],"/PepsiCo/")

# SBC ####
wd$SBC <- paste0(wd$fe[1],"Allgemein/SBC/")

# customer
wd$fe$CCEP <- paste0(wd$fe[1],"BEV_CCEP/")
wd$fe$ABInBev <- paste0(wd$fe[1],"BEV_ABInBev/")
wd$fe$Krombacher <- paste0(wd$fe[1],"BEV_Krombacher/")
wd$fe$MEG <- paste0(wd$fe[1],"BEV_MEG/")
wd$fe$Pepsi <- paste0(wd$fe[1],"BEV_PepsiCo/")

# Einsatz
wd$fe$Einsatz <- paste0(wd$fe[1],"Allgemein/Einsatz/")

# Einsatz
wd$fe$fp <- paste0(wd$fe[1],"Allgemein/Fingerprint/")
wd$fe$spc.validierung <- paste0(wd$fe[1],"Allgemein/Spektrenvalidierung/")

# SQL
wd$sql <- paste0(wd$fe[1],"Allgemein/SQL/")
wd$sql$LG3_spec <- paste0(wd$sql[1],"LG3_specs/")
wd$sql$LG3 <- paste0(wd$sql[1],"LG3/")

wd$sql$CCEP <- paste0(wd$sql[1],"CCEP/")

wd$sql$CCEP$Dorsten <- paste0(wd$sql$CCEP[1],"Dorsten/")
wd$sql$CCEP$Dorsten <- paste0(wd$sql$CCEP[1],"Dorsten/")
wd$sql$CCEP$Moenchengladbach<- paste0(wd$sql$CCEP[1],"Moenchengladbach/")
wd$sql$CCEP$Mannheim <- paste0(wd$sql$CCEP[1],"Mannheim/")

wd$sql$CCEP$Dorsten$DS <- paste0(wd$sql$CCEP$Dorsten[1],"DS/")
wd$sql$CCEP$Dorsten$DC <- paste0(wd$sql$CCEP$Dorsten[1],"DC/")
wd$sql$CCEP$Moenchengladbach$G9 <- paste0(wd$sql$CCEP$Moenchengladbach[1],"G9/")
wd$sql$CCEP$Mannheim$MY <- paste0(wd$sql$CCEP$Mannheim[1],"MY/")

# CCEP #
wd$fe$CCEP$Dorsten <- paste0(wd$fe$CCEP[1],"Dorsten/")
wd$fe$CCEP$Genshagen <- paste0(wd$fe$CCEP[1],"Genshagen/")
wd$fe$CCEP$Mannheim <- paste0(wd$fe$CCEP[1],"Mannheim/")
wd$fe$CCEP$Moenchengladbach <- paste0(wd$fe$CCEP[1],"Moenchengladbach/")
wd$fe$CCEP$Karlsruhe <- paste0(wd$fe$CCEP[1],"Karlsruhe/")
wd$fe$CCEP$Hildesheim <- paste0(wd$fe$CCEP[1],"Hildesheim/")

wd$fe$CCEP$Mastermodelle <- paste0(wd$fe$CCEP[1],"Mastermodelle/")

wd$fe$CCEP$Rezeptueberpruefung <- paste0(wd$fe$CCEP[1],"Rezeptueberpruefung/")

wd$fe$CCEP$Dorsten$DS <- paste0(wd$fe$CCEP$Dorsten[1],"P130070_LG3_DS/")
wd$fe$CCEP$Dorsten$DC <- paste0(wd$fe$CCEP$Dorsten[1],"P130470_LG3_DC/")

wd$fe$CCEP$Genshagen$G2 <- paste0(wd$fe$CCEP$Genshagen[1],"P110880_LG2_G2/")
wd$fe$CCEP$Genshagen$G3 <- paste0(wd$fe$CCEP$Genshagen[1],"P110980_LG2_G3/")
wd$fe$CCEP$Genshagen$G6 <- paste0(wd$fe$CCEP$Genshagen[1],"P110380_LG2_G6/")

wd$fe$CCEP$Mannheim$MY <- paste0(wd$fe$CCEP$Mannheim[1],"P130280_LG3_Gen3/")

wd$fe$CCEP$Moenchengladbach$G9 <- paste0(wd$fe$CCEP$Moenchengladbach[1],"P130180_LiqueGuard_Gen3/")

wd$fe$CCEP$Karlsruhe$LG <- paste0(wd$fe$CCEP$Karlsruhe[1],"P110580_BCANKD/")

# MEG
wd$fe$MEG$Mastermodelle <- paste0(wd$fe$MEG[1],"Mastermodelle/")

wd$fe$MEG$Woerth <- paste0(wd$fe$MEG[1],"Woerth/")
wd$fe$MEG$Kirkel <- paste0(wd$fe$MEG[1],"Kirkel/")
wd$fe$MEG$Loeningen <- paste0(wd$fe$MEG[1],"Loeningen/")

wd$fe$MEG$Woerth$A3 <- paste0(wd$fe$MEG$Woerth[1],"P110780_LG2_A3/")
wd$fe$MEG$Woerth$A5 <- paste0(wd$fe$MEG$Woerth[1],"P110280_LG2_A5/")
wd$fe$MEG$Woerth$A6 <- paste0(wd$fe$MEG$Woerth[1],"P110680_LG2_A6/")

wd$fe$MEG$Loeningen$BLT31_32 <- paste0(wd$fe$MEG$Loeningen[1],"P105160_BLT31_32/")

# Pepsi
wd$fe$Pepsi$Nieder_Roden <- paste0(wd$fe$Pepsi[1], "Nieder_Roden/P110160_L3_PET_CSD/")
wd$fe$Pepsi$Mastermodelle <- paste0(wd$fe$Pepsi[1],"Mastermodelle/")
wd$fe$Pepsi$Rezeptueberpruefung <- paste0(wd$fe$Pepsi[1],"Rezeptueberpruefung/")

# ABInBev
wd$fe$ABInBev$Leuven <- paste0(wd$fe$ABInBev[1],"Leuven/")
wd$fe$ABInBev$St_Louis <- paste0(wd$fe$ABInBev[1],"St_Louis/")

wd$fe$ABInBev$Leuven$BG <- paste0(wd$fe$ABInBev$Leuven[1],"P190680_BG/")

# beverage ####
wd$bev.ccep <- list.dirs(wd$fe$CCEP$Mastermodelle, full.names = T, recursive = F)
wd$bev.ccep.basename <- list.dirs(wd$fe$CCEP$Mastermodelle, full.names = F, recursive = F)
wd$bev.ccep <- wd$bev.ccep[which(is.na(as.numeric(substr(wd$bev.ccep.basename,1,1))))]
wd$bev.ccep.basename <- wd$bev.ccep.basename[which(is.na(as.numeric(substr(wd$bev.ccep.basename,1,1))))]
wd$bev.ccep <- wd$bev.ccep[!wd$bev.ccep.basename %in% "Archiv"]
wd$bev.ccep.basename <- wd$bev.ccep.basename[!wd$bev.ccep.basename %in% "Archiv"]
wd$fe$CCEP$bev <- data.frame(t(wd$bev.ccep))
colnames(wd$fe$CCEP$bev) <- wd$bev.ccep.basename

wd$bev.ccep <- list.dirs(wd$fe$CCEP$Mastermodelle, full.names = T, recursive = F)
wd$bev.ccep.basename <- list.dirs(wd$fe$CCEP$Mastermodelle, full.names = F, recursive = F)
wd$bev.ccep <- wd$bev.ccep[which(is.na(as.numeric(substr(wd$bev.ccep.basename,1,1))))]
wd$bev.ccep.basename <- wd$bev.ccep.basename[which(is.na(as.numeric(substr(wd$bev.ccep.basename,1,1))))]
wd$bev.ccep <- wd$bev.ccep[!wd$bev.ccep.basename %in% "Archiv"]
wd$bev.ccep.basename <- wd$bev.ccep.basename[!wd$bev.ccep.basename %in% "Archiv"]
wd$fe$CCEP$bev <- data.frame(t(wd$bev.ccep))
colnames(wd$fe$CCEP$bev) <- wd$bev.ccep.basename

wd$bev.meg <- list.dirs(wd$fe$MEG$Mastermodelle, full.names = T, recursive = F)
wd$bev.meg.basename <- list.dirs(wd$fe$MEG$Mastermodelle, full.names = F, recursive = F)
# wd$bev.meg <- wd$bev.meg[which(is.na(as.numeric(substr(wd$bev.meg.basename,1,1))))]
# wd$bev.meg.basename <- wd$bev.meg.basename[which(is.na(as.numeric(substr(wd$bev.meg.basename,1,1))))]
# wd$bev.meg <- wd$bev.meg[!wd$bev.meg.basename %in% "Archiv"]
# wd$bev.meg.basename <- wd$bev.meg.basename[!wd$bev.meg.basename %in% "Archiv"]
wd$fe$MEG$bev <- data.frame(t(wd$bev.meg))
colnames(wd$fe$MEG$bev) <- wd$bev.meg.basename

wd$bev.meg <- list.dirs(wd$fe$Pepsi$Mastermodelle, full.names = T, recursive = F)
wd$bev.pepsi.basename <- list.dirs(wd$fe$Pepsi$Mastermodelle, full.names = F, recursive = F)
# wd$bev.pepsi <- wd$bev.pepsi[which(is.na(as.numeric(substr(wd$bev.pepsi.basename,1,1))))]
# wd$bev.pepsi.basename <- wd$bev.pepsi.basename[which(is.na(as.numeric(substr(wd$bev.pepsi.basename,1,1))))]
wd$bev.pepsi <- wd$bev.pepsi[!wd$bev.pepsi.basename %in% "Archiv"]
wd$bev.pepsi.basename <- wd$bev.pepsi.basename[!wd$bev.pepsi.basename %in% "Archiv"]
wd$fe$Pepsi$bev <- data.frame(t(wd$bev.pepsi))
colnames(wd$fe$Pepsi$bev) <- wd$bev.pepsi.basename

# Service ####
wd$service <- paste0(Sys.getenv("OneDriveCommercial"), "/Service_neu/")

# customer
wd$service$CCEP <- paste0(wd$service[1],"BEV_CCEP/")
wd$service$MEG <- paste0(wd$service[1],"BEV_MEG/")

# CCEP #
wd$service$CCEP$Dorsten <- paste0(wd$service$CCEP[1],"CCEP_DOR/")
wd$service$CCEP$Genshagen <- paste0(wd$service$CCEP[1],"CCEP_GEN/")
wd$service$CCEP$Mannheim <- paste0(wd$service$CCEP[1],"CCEP_MAN/")
wd$service$CCEP$Moenchengladbach <- paste0(wd$service$CCEP[1],"CCEP_MOG/")
wd$service$CCEP$Karlsruhe <- paste0(wd$service$CCEP[1],"CCEP_KAR/")

wd$service$CCEP$Dorsten$DS <- paste0(wd$service$CCEP$Dorsten[1],"P130070_LG3_DOR_DS/")
wd$service$CCEP$Dorsten$DC <- paste0(wd$service$CCEP$Dorsten[1],"P130470_LG3_DOR_DC/")

wd$service$CCEP$Genshagen$G2 <- paste0(wd$service$CCEP$Genshagen[1],"P110860_LiquiGuard_CCEG_Genshagen_G2/")
wd$service$CCEP$Genshagen$G3 <- paste0(wd$service$CCEP$Genshagen[1],"P110960_LiquiGuard_CCEG_Genshagen_G3/")
wd$service$CCEP$Genshagen$G6 <- paste0(wd$service$CCEP$Genshagen[1],"P110360_LiquiGuard_CCEG_Genshagen_G6/")

wd$service$CCEP$Mannheim$MY <- paste0(wd$service$CCEP$Mannheim[1],"P130270_LG3_MAN/")

wd$service$CCEP$Moenchengladbach$G9 <- paste0(wd$service$CCEP$Moenchengladbach[1],"P130160_LG3_MOG_G9/")

wd$service$CCEP$Karlsruhe$LG <- paste0(wd$service$CCEP$Karlsruhe[1],"P110560_LiquiGuard_CCEP_KAR/")

# MEG
wd$service$MEG$Woerth <- paste0(wd$service$MEG[1],"MEG_Woerth_am_Rhein/")
wd$service$MEG$Kirkel <- paste0(wd$service$MEG[1],"MEG_Kirkel/")
wd$service$MEG$Loeningen <- paste0(wd$service$MEG[1],"MEG_Loeningen/")

wd$service$MEG$Woerth$A3 <- paste0(wd$service$MEG$Woerth[1],"P110770_LiquiGuard_PEG_Woerth_A3/")
wd$service$MEG$Woerth$A5 <- paste0(wd$service$MEG$Woerth[1],"P110270_LiquiGuard_PEG_Woerth_A5/")
wd$service$MEG$Woerth$A6 <- paste0(wd$service$MEG$Woerth[1],"P110670_LiquiGuard_PEG_Woerth_A6/")

wd$service$MEG$Loeningen$BLT31_32 <- paste0(wd$service$MEG$Loeningen[1],"P105160_SyrupGuard/")
wd$service$MEG$Loeningen$LG <- paste0(wd$service$MEG$Loeningen[1],"P110060_LiquiGuard/")

wd$service$MEG$Kirkel$LG <- paste0(wd$service$MEG$Kirkel[1],"P111060_KEG_Linie 4/")

# ServiceBackup ####
wd$ServiceBackup2 <- "//DC-01/Datenverzeichnis_2/ServiceBackup/"

# VirtualBox ####
wd$VBox <- paste0(wd$ServiceBackup2, "[SPC_CSV]/Vbox_MK/")

# Service Backup ####
wd$servicebackup <-paste0(wd$ServiceBackup2, "[SPC_CSV]/")
wd$servicebackup$unzip <- paste0(wd$servicebackup,"R/zip/")
# customer
wd$servicebackup$CCEP <- paste0(wd$servicebackup[1],"CCEP/")
wd$servicebackup$MEG <- paste0(wd$servicebackup[1],"MEG/")
wd$servicebackup$Pepsi <- paste0(wd$servicebackup[1],"PepsiCo/")
wd$servicebackup$CapriSun$Eppelheim <- paste0(wd$servicebackup[1],"CapriSun/Eppelheim/")

wd$servicebackup$CCEP$Dorsten <- paste0(wd$servicebackup$CCEP[1],"DOR/")
wd$servicebackup$CCEP$Genshagen <- paste0(wd$servicebackup$CCEP[1],"GEN/")
wd$servicebackup$CCEP$Mannheim <- paste0(wd$servicebackup$CCEP[1],"MAN/")
wd$servicebackup$CCEP$Moenchengladbach <- paste0(wd$servicebackup$CCEP[1],"MOG/")
wd$servicebackup$CCEP$Karlsruhe <- paste0(wd$servicebackup$CCEP[1],"KAR/")

wd$servicebackup$CCEP$Dorsten$DS <- paste0(wd$servicebackup$CCEP$Dorsten[1],"DS/")
wd$servicebackup$CCEP$Dorsten$DC <- paste0(wd$servicebackup$CCEP$Dorsten[1],"DC/")

wd$servicebackup$CCEP$Genshagen$G2 <- paste0(wd$servicebackup$CCEP$Genshagen[1],"G2/")
wd$servicebackup$CCEP$Genshagen$G3 <- paste0(wd$servicebackup$CCEP$Genshagen[1],"G3/")
wd$servicebackup$CCEP$Genshagen$G6 <- paste0(wd$servicebackup$CCEP$Genshagen[1],"G6/")

wd$servicebackup$CCEP$Mannheim$MY <- paste0(wd$servicebackup$CCEP$Mannheim[1],"MY/")

wd$servicebackup$CCEP$Moenchengladbach$G9 <- paste0(wd$servicebackup$CCEP$Moenchengladbach[1],"G9/")

wd$servicebackup$CCEP$Karlsruhe$BCANKD <- paste0(wd$servicebackup$CCEP$Karlsruhe[1],"BCANKD/")

# MEG
wd$servicebackup$MEG$Woerth <- paste0(wd$servicebackup$MEG[1],"MEG_Woerth/")
wd$servicebackup$MEG$Kirkel <- paste0(wd$servicebackup$MEG[1],"MEG_Kirkel/")
wd$servicebackup$MEG$Loeningen <- paste0(wd$servicebackup$MEG[1],"MEG_Loeningen/")

wd$servicebackup$MEG$Woerth$A3 <- paste0(wd$servicebackup$MEG$Woerth[1],"A3/")
wd$servicebackup$MEG$Woerth$A5 <- paste0(wd$servicebackup$MEG$Woerth[1],"A5/")
wd$servicebackup$MEG$Woerth$A6 <- paste0(wd$servicebackup$MEG$Woerth[1],"A6/")

wd$servicebackup$MEG$Loeningen$BLT31_32 <- paste0(wd$servicebackup$MEG$Loeningen[1],"BLT31_32/")
wd$servicebackup$MEG$Loeningen$LG_LOE <- paste0(wd$servicebackup$MEG$Loeningen[1],"LG100i/")

wd$servicebackup$MEG$Kirkel$A4 <- paste0(wd$servicebackup$MEG$Kirkel[1],"A4/")

# Pepsi
wd$servicebackup$Pepsi$Nieder_Roden <- paste0(wd$servicebackup$Pepsi, "Nieder_Roden/")
wd$servicebackup$Pepsi$Nieder_Roden$L3_PET_CSD <- paste0(wd$servicebackup$Pepsi$Nieder_Roden[1], "L3_PET_CSD/")

# CapriSun
wd$servicebackup$CapriSun$Eppelheim$TS1 <- paste0(wd$servicebackup$CapriSun$Eppelheim[[1]],"TS1/")
wd$servicebackup$CapriSun$Eppelheim$TS2 <- paste0(wd$servicebackup$CapriSun$Eppelheim[[1]],"TS2/")
wd$servicebackup$CapriSun$Eppelheim$TS3 <- paste0(wd$servicebackup$CapriSun$Eppelheim[[1]],"TS3/")
wd$servicebackup$CapriSun$Eppelheim$TS4 <- paste0(wd$servicebackup$CapriSun$Eppelheim[[1]],"TS4/")
wd$servicebackup$CapriSun$Eppelheim$TS4 <- paste0(wd$servicebackup$CapriSun$Eppelheim[[1]],"TS4/")
wd$servicebackup$CapriSun$Eppelheim$SG500FS <- paste0(wd$servicebackup$CapriSun$Eppelheim[[1]],"SG500FS/")
wd$servicebackup$CapriSun$Eppelheim$TS5 <- wd$servicebackup$CapriSun$Eppelheim$SG500FS

# Bluebox ####
wd$bluebox <- "C:/Users/Labor/Desktop/TIDASDAQ3_Messdaten/"

# Tidas ####
wd$tidas <- paste0(wd$desktop, "TIDASDAQ3_Messdaten/")

# GC-IMS ####
wd$servicebackup$ABInBev$Landau <- paste0(wd$ServiceBackup, "[SPC_CSV]/ABInBev/Landau/")

# Wartung ####
wd$wartung <- paste0(wd$ServiceBackup, "[Wartung]/")
# customer
wd$wartung$CCEP <- paste0(wd$wartung[1],"BEV_CCEP/")
wd$wartung$MEG <- paste0(wd$wartung[1],"BEV_MEG/")
wd$wartung$Pepsi <- paste0(wd$wartung[1],"BEV_PepsiCo/")

wd$wartung$CCEP$Dorsten <- paste0(wd$wartung$CCEP[1],"CCEP_DOR/")
wd$wartung$CCEP$Genshagen <- paste0(wd$wartung$CCEP[1],"CCEP_GEN/")
wd$wartung$CCEP$Mannheim <- paste0(wd$wartung$CCEP[1],"CCEP_MAN/")
wd$wartung$CCEP$Moenchengladbach <- paste0(wd$wartung$CCEP[1],"CCEP_MOG/")
wd$wartung$CCEP$Karlsruhe <- paste0(wd$wartung$CCEP[1],"CCEP_KAR/")

# MEG
wd$wartung$MEG$Woerth <- paste0(wd$wartung$MEG[1],"MEG_Woerth am Rhein/")
wd$wartung$MEG$Kirkel <- paste0(wd$wartung$MEG[1],"MEG_Kirkel/")
wd$wartung$MEG$Loeningen <- paste0(wd$wartung$MEG[1],"MEG_Loeningen/")

# Pepsi
wd$wartung$Pepsi$Nieder_Roden <- paste0(wd$wartung$Pepsi[1], "P110160_Pepsico_Nieder_Roden/")

# CapriSun
wd$wartung$CapriSun$Eppelheim <- paste0(wd$wartung[1], "BEV_CapriSun/")

# Produktion ####
wd$Eingangstest <- paste0(Sys.getenv("OneDriveCommercial"), "/Produktion/Eingangskontrolle/")

wd$Eingangstest$LWL <- paste0(wd$Eingangstest[1],"Lichtwellenleiter/")
wd$Eingangstest$MUX <- paste0(wd$Eingangstest[1],"Kuevettenwechsler/")
wd$Eingangstest$Spektrometer <- paste0(wd$Eingangstest[1],"Spektrometer/")

# Hardware ####
wd$Hardware <- paste0(Sys.getenv("OneDriveCommercial"), "/FE_Hardware/")

wd$Hardware$DTMultimodal <- paste0(wd$Hardware[1], "P902280_BEV_ZIM DTMultimodal/10_Methodenentwicklung/")
wd$Hardware$ABInBev <- paste0(wd$Hardware[1], "P191380_ABInBev_BG/")
wd$Hardware$ABInBev$FlavourSpec <- paste0(wd$Hardware$ABInBev, "03_Projektdokumentation/06_DDM/10_GC-IMS/05_Validierung_ABInBev_FlavourSpec/")
wd$Hardware$ABInBev$GC_IMS_GAS <- paste0(wd$Hardware$ABInBev, "03_Projektdokumentation/06_DDM/10_GC-IMS/04_Validierung_GAS_GC-IMS/")
wd$Hardware$ABInBev$GC_IMS_DT <- paste0(wd$Hardware$ABInBev, "03_Projektdokumentation/06_DDM/10_GC-IMS/06_Validierung_ABInBev_GC-IMS_LD_Prozessversion/")

options(warn=0)

# customer and product ID ####
# dt$customerlist <- read.csv2(paste0(wd$data,"dt_customer.csv"),sep="\t")
# dt$product_ID <- fread(paste0(wd$data,"dt_customer_product_ID.csv"))
