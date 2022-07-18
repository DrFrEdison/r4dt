KA.TSW.QS.FB42.01 <- function( path = getwd(), subfiles = NA){
  
  labfiles <- list()
  
  if( is.na(subfiles))  labfiles$xlsx <- lapply( grep( "KA-TSW-QS-FB42", dir( path = path, pattern = ".xlsx"), value = T), function( x ) read.xlsx( x, sheet = "Vergleichmessung Master Methode") )
  if( !is.na(subfiles))  labfiles$xlsx <- lapply( grep( "KA-TSW-QS-FB42", dir( path = path, pattern = ".xlsx"), value = T)[ subfiles ], function( x ) read.xlsx( x, sheet = "Vergleichmessung Master Methode") )
      
  labfiles$raw <- lapply( labfiles$xlsx, function( x ) x[ !is.na( x$Produkt) , ] )
  
  for(i in 1:length(labfiles$raw)){ 
    labfiles$raw[[ i ]][ , grep( "Probenahme.Datum", colnames( labfiles$raw[[ i ]] ))] <- convertToDate( labfiles$raw[[ i ]][ , grep( "Probenahme.Datum", colnames( labfiles$raw[[ i ]] ))] )
    labfiles$raw[[ i ]][ , grep( "Probenahme.Uhrzeit", colnames( labfiles$raw[[ i ]] ))] <- strftime( convertToDateTime( labfiles$raw[[ i ]][ , grep( "Probenahme.Uhrzeit", colnames( labfiles$raw[[ i ]] ))] ), format = "%H:%M:%S")
    
    labfiles$raw[[ i ]][ , grep( "Analysedatum", colnames( labfiles$raw[[ i ]] ))] <- convertToDate( labfiles$raw[[ i ]][ , grep( "Analysedatum", colnames( labfiles$raw[[ i ]] ))] )
    labfiles$raw[[ i ]][ , grep( "Analysezeit.Labor", colnames( labfiles$raw[[ i ]] ))] <- strftime( convertToDateTime( labfiles$raw[[ i ]][ , grep( "Analysezeit.Labor", colnames( labfiles$raw[[ i ]] ))] ), format = "%H:%M:%S")
  }
  
  labfiles$merge <- rbindlist(labfiles$raw)
  labfiles$merge$datetime <- as.POSIXct( paste(labfiles$merge$Probenahme.Datum, labfiles$merge$Probenahme.Uhrzeit), tz = "UTC")
  labfiles$merge <- labfiles$merge[ , !colnames(labfiles$merge) %in% c("Probe.Nr.", "Volumen.Flasche.(in.l)", "Analysedatum"
                                                                       , "Analysezeit.Labor", "Analysiert.durch", "Messwert.1.Labor.[%]"
                                                                       , "Messwert.2.Labor.[%]", "Probenahme.Datum", "Probenahme.Uhrzeit"
                                                                       , "Kommentare"), with = F]
  
  labfiles$merge <- labfiles$merge[ , moveme( colnames(labfiles$merge), "datetime first"), with = F]

  return(labfiles$merge)
}
