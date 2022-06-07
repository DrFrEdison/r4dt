sourcep <- "I:/";input <- list();input$linux <- F;source(paste0(sourcep,"Allgemein/R_dt_project/R/source_read.R"));input$task <- "read_csv";input$LG <- 3

input$export_directory <- wd$servicebackup$ABInBev$Landau
setwd(input$export_directory)

gas <- list()
gas$raw <- list()
gas$trans$unit <- list()
gas$meafiles <- dir(pattern = "\\.mea$")

gas$mealist <- read.xlsx("gc_ims_landau_overview_all.xlsx")

gas$meafiles <- dir(pattern = "\\.mea$", recursive = T)
gas$meafilesp <- substr(gas$meafiles
                        , lapply(gregexpr("/", gas$meafiles), function(x) x[[1]] + 1)
                        , nchar(gas$meafiles))

gas$missingfiles <- gas$meafiles[ !gas$meafilesp %in% gas$mealist$ID ]
gas$missingfilesp <- substr(gas$missingfiles
                        , lapply(gregexpr("/", gas$missingfiles), function(x) x[[1]] + 1)
                        , nchar(gas$missingfiles))

# read in mea ####
for(i in 1:length(gas$missingfiles)){
  suppressWarnings(gas$raw[[i]] <- readLines(gas$missingfiles[i]))
  gas$raw[[i]] <- gas$raw[[i]][1:grep("nom Drift Tube Length", gas$raw[[i]])]
}

# parameter ####
gas$trans$para <- lapply( 
  gas$raw, function(xx) lapply(
    xx, function(x) 
      trimws(
        strsplit(
          x, "=" 
        )[[1]][1]
      )
  )
)

# values ####
gas$trans$value <- lapply( 
  gas$raw, function(xx) lapply(
    xx, function(x) 
      trimws(
        strsplit(
          x, "="
        )[[1]][2]
      )
  )
)

# units ####
gas$trans$unit <- lapply( 
  gas$trans$value, function(xx) lapply(
    xx, function(x) 
      substr(x , gregexpr("\\[", x)[[1]][1] , gregexpr("\\]", x)[[1]][1]))
)

gas$trans$unit_2 <- lapply( 
  gas$trans$value, function(xx) which(unlist(
    lapply(
      xx, function(x) 
        length(gregexpr("\\[", x)[[1]]))
  ) != 1)
)

gas$trans$unit_2_text <- lapply(
  mapply( function(x, y) 
    x[y], x = gas$trans$value, y = gas$trans$unit_2) , function(xx) 
      ifelse(length(xx) > 0, strsplit(unlist(xx), ";"), xx)
)

# value without unit ####
gas$trans$value <- lapply(
  gas$trans$value, function(xx)
    lapply(xx, function(x) 
      substr(x, 1, 
             ifelse(gregexpr("\\[", x)[[1]] > 0, gregexpr("\\[", x)[[1]], nchar(x))
      )
    )
)

gas$trans$value <- lapply(gas$trans$value, function(x) trimws(gsub("\\[", "", x)))

# extract gc_ims info ####
gcims <- list()

gcims$Flow_EPC_1 <- lapply(mapply(function(x, y)
  y[grep("Flow Epc 1", x)], 
  x = gas$trans$para, y = gas$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Flow_EPC_1 <- lapply(gcims$Flow_EPC_1, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Flow_EPC_1_mean <- lapply(gcims$Flow_EPC_1, mean)
gcims$Flow_EPC_1_sd <- lapply(gcims$Flow_EPC_1, sd)

gcims$Flow_EPC_2 <- lapply(mapply(function(x, y)
  y[grep("Flow Epc 2", x)], 
  x = gas$trans$para, y = gas$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Flow_EPC_2 <- lapply(gcims$Flow_EPC_2, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Flow_EPC_2_mean <- lapply(gcims$Flow_EPC_2, mean)
gcims$Flow_EPC_2_sd <- lapply(gcims$Flow_EPC_2, sd)

gcims$Flow_Pressure_Ambient <- lapply(mapply(function(x, y)
  y[grep("Pressure Ambient", x)], 
  x = gas$trans$para, y = gas$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Flow_Pressure_Ambient <- lapply(gcims$Flow_Pressure_Ambient, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Flow_Pressure_Ambient_mean <- lapply(gcims$Flow_Pressure_Ambient, mean)
gcims$Flow_Pressure_Ambient_sd <- lapply(gcims$Flow_Pressure_Ambient, sd)

gcims$Pressure_EPC_1 <- lapply(mapply(function(x, y)
  y[grep("Pressure Epc 1", x)], 
  x = gas$trans$para, y = gas$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Pressure_EPC_1 <- lapply(gcims$Pressure_EPC_1, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Pressure_EPC_1_mean <- lapply(gcims$Pressure_EPC_1, mean)
gcims$Pressure_EPC_1_sd <- lapply(gcims$Pressure_EPC_1, sd)

gcims$Pressure_EPC_2 <- lapply(mapply(function(x, y)
  y[grep("Pressure Epc 2", x)], 
  x = gas$trans$para, y = gas$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Pressure_EPC_2 <- lapply(gcims$Pressure_EPC_2, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Pressure_EPC_2_mean <- lapply(gcims$Pressure_EPC_2, mean)
gcims$Pressure_EPC_2_sd <- lapply(gcims$Pressure_EPC_2, sd)

gcims$Snapshot <- mapply(function(x, y)
  y[grep("Snapshot", x)], 
  x = gas$trans$para, y = gas$trans$value)
gcims$Snapshot_Current_Valus <- mapply(function(x, y)
  y[grep("Snapshot Current Values", x)], 
  x = gas$trans$para, y = gas$trans$value)

# Diacetyl Pentanedione ####
gcims$Substances <- mapply(function(x, y)
  y[grep("Recognized substances", x)], 
  x = gas$trans$para, y = gas$trans$value)

gcims$DIAC <- 
  as.numeric(substr(gcims$Substances
                    , as.numeric(gregexpr("diacetyl" , gcims$Substances)) + nchar("diacetyl") + 1
                    , mapply(function(x, y) x[which(x > y)[1]] - 1
                             , x = gregexpr(" " , gcims$Substances), y = as.numeric(gregexpr("diacetyl" , gcims$Substances)) + nchar("diacetyl"))
  )
  )

gcims$PENT <- 
  as.numeric(substr(gcims$Substances
                    , as.numeric(gregexpr("pentandione" , gcims$Substances)) + nchar("pentandione") + 1
                    , mapply(function(x, y) x[which(x > y)[1]] - 1
                             , x = gregexpr(" " , gcims$Substances), y = as.numeric(gregexpr("pentandione" , gcims$Substances)) + nchar("pentandione"))
  )
  )

gcims$PENT2 <- 
  as.numeric(substr(gcims$Substances
                    , as.numeric(gregexpr("pentadione" , gcims$Substances)) + nchar("pentadione") + 1
                    , mapply(function(x, y) x[which(x > y)[1]] - 1
                             , x = gregexpr(" " , gcims$Substances), y = as.numeric(gregexpr("pentadione" , gcims$Substances)) + nchar("pentadione"))
  )
  )

gcims$PENT[which(!is.na(gcims$PENT2))] <- gcims$PENT2[which(!is.na(gcims$PENT2))]

# merge ####
gas$trans$value <- lapply(gas$trans$value, function(x) gsub('\"', "", x))

for(i in 1:length(gas$trans$value)){
  gas$trans$value[[i]][  grep("Flow Epc 1", gas$trans$para[[i]])  ] <- paste0(round(gcims$Flow_EPC_1_mean[[i]],1), " ± ", round(gcims$Flow_EPC_1_sd[[i]],1))
  gas$trans$value[[i]][  grep("Flow Epc 2", gas$trans$para[[i]])  ] <- paste0(round(gcims$Flow_EPC_2_mean[[i]],1), " ± ", round(gcims$Flow_EPC_2_sd[[i]],1))
  gas$trans$value[[i]][  grep("Pressure Ambient", gas$trans$para[[i]])  ] <- paste0(round(gcims$Flow_Pressure_Ambient_mean[[i]],1), " ± ", round(gcims$Flow_Pressure_Ambient_sd[[i]],1))
  gas$trans$value[[i]][  grep("Pressure Epc 1", gas$trans$para[[i]])  ] <- paste0(round(gcims$Pressure_EPC_1_mean[[i]],1), " ± ", round(gcims$Pressure_EPC_1_sd[[i]],1))
  gas$trans$value[[i]][  grep("Pressure Epc 2", gas$trans$para[[i]])  ] <- paste0(round(gcims$Pressure_EPC_2_mean[[i]],1), " ± ", round(gcims$Pressure_EPC_1_sd[[i]],1))
  
  gas$trans$value[[i]][  grep("Timestamp", gas$trans$para[[i]])  ] <- 
    as.character(as.POSIXct(gsub("T", " ", gas$trans$value[[i]][  grep("Timestamp", gas$trans$para[[i]])  ])
                            , format = "%Y-%m-%d %H:%M:%S"
                            , tz = as.character(gas$trans$value[[i]][  grep("Timezone", gas$trans$para[[i]])  ])))
  
  if(length(grep("Snapshot Current Values", gas$trans$para[[i]])) > 0) gas$trans$value[[i]][  grep("Snapshot Current Values", gas$trans$para[[i]])  ]  <- NA
  if(length(grep("Snapshot", gas$trans$para[[i]])) > 0) gas$trans$value[[i]][  grep("Snapshot", gas$trans$para[[i]])  ] <- NA
  
  if(length(grep("Recognized substances", gas$trans$para[[i]])) > 0) gas$trans$value[[i]][  grep("Recognized substances", gas$trans$para[[i]])  ] <- NA
  
}

gcims$merge <- gas$trans$value # mapply(function(y,z) rbind(y,z), y = gas$trans$unit, z = gas$trans$value)
gcims$unitmerge <- gas$trans$unit
gcims$merge <- lapply(gcims$merge, function(x) data.frame(t(x)))
gcims$unitmerge <- lapply(gcims$unitmerge, function(x) data.frame(t(x)))
for(i in 1:length(gcims$merge)) names(gcims$merge[[i]]) <- gas$trans$para[[i]]
for(i in 1:length(gcims$unitmerge)) names(gcims$unitmerge[[i]]) <- gas$trans$para[[i]]

require(plyr)
gcims$merge <- rbind.fill(gcims$merge)
gcims$unitmerge <- rbind.fill(gcims$unitmerge)

gcims$merge <- gcims$merge[ , -which(names(gcims$merge) %in% c("Recognized substances", "Snapshot Start", "Start temp 4", "Start temp 6","Snapshot Current Values", "Status comment", "Sample"))]
gcims$unitmerge <- gcims$unitmerge[ , -which(names(gcims$unitmerge) %in% c("Recognized substances", "Snapshot Start", "Start temp 4", "Start temp 6","Snapshot Current Values", "Status comment", "Sample"))]

gcims$merge$Diacetyl = gcims$DIAC
gcims$merge$Pentanedione = gcims$PENT
gcims$merge$ID = gas$missingfilesp

gcims$unitmerge$Diacetyl = "[µg/L]"
gcims$unitmerge$Pentanedione = "[µg/L]"
gcims$unitmerge$ID = ""

names(gcims$merge) <- gsub(" ", "_", names(gcims$merge))
names(gcims$unitmerge) <- gsub(" ", "_", names(gcims$unitmerge))

head(gcims$merge)
gcims$merge <- gcims$merge[ , .moveme(names(gcims$merge), "Timestamp ID Diacetyl Pentanedione first")]
gcims$merge <- gcims$merge[ , .moveme(names(gcims$merge), "ADIO_name ADIO_serial ADIO_version Class Drift_Gas Filter Firmware_date Firmware_version GC_Column Machine_name Machine_serial Machine_type last")]
gcims$unitmerge <- gcims$unitmerge[ , .moveme(names(gcims$unitmerge), "Timestamp ID Diacetyl Pentanedione first")]
gcims$unitmerge <- gcims$unitmerge[ , .moveme(names(gcims$unitmerge), "ADIO_name ADIO_serial ADIO_version Class Drift_Gas Filter Firmware_date Firmware_version GC_Column Machine_name Machine_serial Machine_type last")]

setwd(input$export_directory)
gcims$merge <- rbind.fill(gas$mealist, gcims$merge)

gcims$merge <- gcims$merge[order(gcims$merge$Timestamp, decreasing = T) , ]
gcims$zero <- gcims$merge[which(gcims$merge$Diacetyl < 0),]
gcims$nozero <- gcims$merge[which(gcims$merge$Diacetyl >= 0),]

gcims$merge <- rbind(unlist(apply(gcims$unitmerge, 2, unique)), gcims$merge)
gcims$zero <- rbind(unlist(apply(gcims$unitmerge, 2, unique)), gcims$zero)
gcims$nozero <- rbind(unlist(apply(gcims$unitmerge, 2, unique)), gcims$nozero)

write.xlsx(gcims$merge, "gc_ims_landau_overview_all.xlsx", row.names = F)
write.xlsx(gcims$nozero, "gc_ims_landau_overview_nozero.xlsx", row.names = F)
write.xlsx(gcims$zero, "gc_ims_landau_overview_zero.xlsx", row.names = F)

dir(paste0(input$export_directory, "mea"), pattern = "\\.mea$") [ gcims$zero$ID %in% dir(paste0(input$export_directory, "mea"), pattern = "\\.mea$") ]

file.rename(paste0("mea/", gcims$zero$ID[which( gcims$zero$ID %in% dir(paste0(input$export_directory, "mea"), pattern = "\\.mea$") )])
            , paste0("mea0/", gcims$zero$ID[which( gcims$zero$ID %in% dir(paste0(input$export_directory, "mea"), pattern = "\\.mea$") )]))

