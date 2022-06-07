.sql_path <- function(customer, location, unit){
  sql_path <- list()
  
  sql_path$a <- which(names(wd$sql)==customer)
  if(length(sql_path$a)==0) stop("Wrong customer chosen")
  
  sql_path$b <- which(names(wd$sql[[sql_path$a]])==location)
  if(length(sql_path$b)==0) stop("Wrong location chosen")
  
  sql_path$c <- which(names(wd$sql[[sql_path$a]][[sql_path$b]])==unit)
  if(length(sql_path$c)==0) stop("Wrong unit chosen")
  
  return(wd$sql[[sql_path$a]][[sql_path$b]][[sql_path$c]])
}
