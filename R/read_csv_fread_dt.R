freadr4dt <- function( csvfile){
  
  csvfile <- gsub(".csv.csv", ".csv", paste0(csvfile, ".csv"))
  
  csvfile <- fread(csvfile, sep = ";", dec = ",")
  
  ppp <- transfer_csv.num.col(csvfile)
  
  csvfile.names <- colnames(csvfile)
  
  csvfile.names[ ppp$numcol ] <- paste0( "X", csvfile.names[ ppp$numcol ])
  
  setnames(csvfile, new = csvfile.names)
  
  return(csvfile)
}
