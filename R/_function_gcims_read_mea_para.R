sourcep <- "I:/";input <- list();input$linux <- F;source(paste0(sourcep,"Allgemein/R_dt_project/R/source_read.R"));input$task <- "read_csv";input$LG <- 3

input$export_directory <- wd$servicebackup$ABInBev$Landau
setwd(input$export_directory)
setwd("./mea")

filep <- list() 
filep$raw <- list()
filep$trans$unit <- list()
filep$name <- dir(pattern = "\\.mea$")

# read in mea ####
for(i in 1:length(filep$name)){
  suppressWarnings(filep$raw[[i]] <- readLines(filep$name[i]))
  filep$raw[[i]] <- filep$raw[[i]][1:grep("nom Drift Tube Length", filep$raw[[i]])]
}

# parameter ####
filep$trans$para <- lapply( 
  filep$raw, function(xx) lapply(
    xx, function(x) 
      trimws(
        strsplit(
          x, "=" 
        )[[1]][1]
      )
  )
)

# values ####
filep$trans$value <- lapply( 
  filep$raw, function(xx) lapply(
    xx, function(x) 
      trimws(
        strsplit(
          x, "="
        )[[1]][2]
      )
  )
)

# units ####
filep$trans$unit <- lapply( 
  filep$trans$value, function(xx) lapply(
    xx, function(x) 
      substr(x , gregexpr("\\[", x)[[1]][1] , gregexpr("\\]", x)[[1]][1]))
)

filep$trans$unit_2 <- lapply( 
  filep$trans$value, function(xx) which(unlist(
    lapply(
      xx, function(x) 
        length(gregexpr("\\[", x)[[1]]))
  ) != 1)
)

filep$trans$unit_2_text <- lapply(
  mapply( function(x, y) 
    x[y], x = filep$trans$value, y = filep$trans$unit_2) , function(xx) 
      ifelse(length(xx) > 0, strsplit(unlist(xx), ";"), xx)
)

# value without unit ####
filep$trans$value <- lapply(
  filep$trans$value, function(xx)
  lapply(xx, function(x) 
    substr(x, 1, 
           ifelse(gregexpr("\\[", x)[[1]] > 0, gregexpr("\\[", x)[[1]], nchar(x))
           )
    )
  )

filep$trans$value <- lapply(filep$trans$value, function(x) trimws(gsub("\\[", "", x)))

# extract gc_ims info ####
gcims <- list()
 
gcims$Flow_EPC_1 <- lapply(mapply(function(x, y)
  y[grep("Flow Epc 1", x)], 
  x = filep$trans$para, y = filep$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Flow_EPC_1 <- lapply(gcims$Flow_EPC_1, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Flow_EPC_1_mean <- lapply(gcims$Flow_EPC_1, mean)
gcims$Flow_EPC_1_sd <- lapply(gcims$Flow_EPC_1, sd)

gcims$Flow_EPC_2 <- lapply(mapply(function(x, y)
  y[grep("Flow Epc 2", x)], 
  x = filep$trans$para, y = filep$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Flow_EPC_2 <- lapply(gcims$Flow_EPC_2, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Flow_EPC_2_mean <- lapply(gcims$Flow_EPC_2, mean)
gcims$Flow_EPC_2_sd <- lapply(gcims$Flow_EPC_2, sd)

gcims$Flow_Pressure_Ambient <- lapply(mapply(function(x, y)
  y[grep("Pressure Ambient", x)], 
  x = filep$trans$para, y = filep$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Flow_Pressure_Ambient <- lapply(gcims$Flow_Pressure_Ambient, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Flow_Pressure_Ambient_mean <- lapply(gcims$Flow_Pressure_Ambient, mean)
gcims$Flow_Pressure_Ambient_sd <- lapply(gcims$Flow_Pressure_Ambient, sd)

gcims$Pressure_EPC_1 <- lapply(mapply(function(x, y)
  y[grep("Pressure Epc 1", x)], 
  x = filep$trans$para, y = filep$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Pressure_EPC_1 <- lapply(gcims$Pressure_EPC_1, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Pressure_EPC_1_mean <- lapply(gcims$Pressure_EPC_1, mean)
gcims$Pressure_EPC_1_sd <- lapply(gcims$Pressure_EPC_1, sd)

gcims$Pressure_EPC_2 <- lapply(mapply(function(x, y)
  y[grep("Pressure Epc 2", x)], 
  x = filep$trans$para, y = filep$trans$value), function(xx) strsplit(xx, " ")
)

gcims$Pressure_EPC_2 <- lapply(gcims$Pressure_EPC_2, function(x) as.numeric(as.character(gsub('\"', "", unlist(x), fixed = TRUE))))
gcims$Pressure_EPC_2_mean <- lapply(gcims$Pressure_EPC_2, mean)
gcims$Pressure_EPC_2_sd <- lapply(gcims$Pressure_EPC_2, sd)

gcims$Snapshot <- mapply(function(x, y)
  y[grep("Snapshot", x)], 
  x = filep$trans$para, y = filep$trans$value)
gcims$Snapshot_Current_Valus <- mapply(function(x, y)
  y[grep("Snapshot Current Values", x)], 
  x = filep$trans$para, y = filep$trans$value)

# Diacetyl Pentanedione ####
gcims$Substances <- mapply(function(x, y)
  y[grep("Recognized substances", x)], 
  x = filep$trans$para, y = filep$trans$value)

unique(unlist(lapply(gcims$Substances, function(x)  substr(x, 2, unlist(gregexpr(":", gcims$Substances[[16]]))[1] - 1))))
unique(unlist(lapply(gcims$Substances, function(x)  substr(x, unlist(gregexpr(",", x))[1] + 2, unlist(gregexpr(":", x))[2] - 1))))

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
filep$trans$value <- lapply(filep$trans$value, function(x) gsub('\"', "", x))

for(i in 1:length(filep$trans$value)){
  filep$trans$value[[i]][  grep("Flow Epc 1", filep$trans$para[[i]])  ] <- paste0(round(gcims$Flow_EPC_1_mean[[i]],1), " ± ", round(gcims$Flow_EPC_1_sd[[i]],1))
  filep$trans$value[[i]][  grep("Flow Epc 2", filep$trans$para[[i]])  ] <- paste0(round(gcims$Flow_EPC_2_mean[[i]],1), " ± ", round(gcims$Flow_EPC_2_sd[[i]],1))
  filep$trans$value[[i]][  grep("Pressure Ambient", filep$trans$para[[i]])  ] <- paste0(round(gcims$Flow_Pressure_Ambient_mean[[i]],1), " ± ", round(gcims$Flow_Pressure_Ambient_sd[[i]],1))
  filep$trans$value[[i]][  grep("Pressure Epc 1", filep$trans$para[[i]])  ] <- paste0(round(gcims$Pressure_EPC_1_mean[[i]],1), " ± ", round(gcims$Pressure_EPC_1_sd[[i]],1))
  filep$trans$value[[i]][  grep("Pressure Epc 2", filep$trans$para[[i]])  ] <- paste0(round(gcims$Pressure_EPC_2_mean[[i]],1), " ± ", round(gcims$Pressure_EPC_1_sd[[i]],1))
  
  filep$trans$value[[i]][  grep("Timestamp", filep$trans$para[[i]])  ] <- 
    as.character(as.POSIXct(gsub("T", " ", filep$trans$value[[i]][  grep("Timestamp", filep$trans$para[[i]])  ])
               , format = "%Y-%m-%d %H:%M:%S"
               , tz = as.character(filep$trans$value[[i]][  grep("Timezone", filep$trans$para[[i]])  ])))
  
  if(length(grep("Snapshot Current Values", filep$trans$para[[i]])) > 0) filep$trans$value[[i]][  grep("Snapshot Current Values", filep$trans$para[[i]])  ]  <- NA
  if(length(grep("Snapshot", filep$trans$para[[i]])) > 0) filep$trans$value[[i]][  grep("Snapshot", filep$trans$para[[i]])  ] <- NA
  
  if(length(grep("Recognized substances", filep$trans$para[[i]])) > 0) filep$trans$value[[i]][  grep("Recognized substances", filep$trans$para[[i]])  ] <- NA
  
}

gcims$merge <- filep$trans$value # mapply(function(y,z) rbind(y,z), y = filep$trans$unit, z = filep$trans$value)
gcims$unitmerge <- filep$trans$unit
gcims$merge <- lapply(gcims$merge, function(x) data.frame(t(x)))
gcims$unitmerge <- lapply(gcims$unitmerge, function(x) data.frame(t(x)))
for(i in 1:length(gcims$merge)) names(gcims$merge[[i]]) <- filep$trans$para[[i]]
for(i in 1:length(gcims$unitmerge)) names(gcims$unitmerge[[i]]) <- filep$trans$para[[i]]

require(plyr)
gcims$merge <- rbind.fill(gcims$merge)
gcims$unitmerge <- rbind.fill(gcims$unitmerge)

gcims$merge <- gcims$merge[ , -which(names(gcims$merge) %in% c("Recognized substances", "Snapshot Start", "Start temp 4", "Start temp 6"," Snapshot Current Values", "Status comment", "Sample"))]
gcims$unitmerge <- gcims$unitmerge[ , -which(names(gcims$unitmerge) %in% c("Recognized substances", "Snapshot Start", "Start temp 4", "Start temp 6"," Snapshot Current Values", "Status comment", "Sample"))]

gcims$merge$Diacetyl = gcims$DIAC
gcims$merge$Pentanedione = gcims$PENT
gcims$merge$ID = filep$name

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
gcims$merge <- gcims$merge[order(gcims$merge$Timestamp, decreasing = T) , ]
gcims$zero <- gcims$merge[which(gcims$merge$Diacetyl < 0),]
gcims$nozero <- gcims$merge[which(gcims$merge$Diacetyl >= 0),]

gcims$merge <- rbind(unlist(apply(gcims$unitmerge, 2, unique)), gcims$merge)
gcims$zero <- rbind(unlist(apply(gcims$unitmerge, 2, unique)), gcims$zero)
gcims$nozero <- rbind(unlist(apply(gcims$unitmerge, 2, unique)), gcims$nozero)

write.xlsx(gcims$merge, "gc_ims_landau_overview_all.xlsx", row.names = F)
write.xlsx(gcims$nozero, "gc_ims_landau_overview_nozero.xlsx", row.names = F)
write.xlsx(gcims$zero, "gc_ims_landau_overview_zero.xlsx", row.names = F)
