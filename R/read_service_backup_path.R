service_backup_path <- function(customer, location, line){
  service_backup_path <- list()

  service_backup_path$a <- which(names(wd$servicebackup)==customer)
  if(length(service_backup_path$a)==0) stop("Wrong customer chosen")

  service_backup_path$b <- which(names(wd$servicebackup[[service_backup_path$a]])==location)
  if(length(service_backup_path$b)==0) stop("Wrong location chosen")

  service_backup_path$c <- which(names(wd$servicebackup[[service_backup_path$a]][[service_backup_path$b]])==line)
  if(length(service_backup_path$c)==0) stop("Wrong line chosen")

  return(wd$servicebackup[[service_backup_path$a]][[service_backup_path$b]][[service_backup_path$c]])
}

service_path <- function(customer, location, line){
  service_path <- list()

  service_path$a <- which(names(wd$service)==customer)
  if(length(service_path$a)==0) stop("Wrong customer chosen")

  service_path$b <- which(names(wd$service[[service_path$a]])==location)
  if(length(service_path$b)==0) stop("Wrong location chosen")

  service_path$c <- which(names(wd$service[[service_path$a]][[service_path$b]])==line)
  if(length(service_path$c)==0) stop("Wrong line chosen")

  return(wd$service[[service_path$a]][[service_path$b]][[service_path$c]])
}


