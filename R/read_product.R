stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

customer.location.by.line <- function(line, customer.list){

  rowp <- which( customer.list$line == line )
  customer.list.sub <- customer.list[ rowp ,]

  customer.list.sub <- list(customer = customer.list.sub$customer
                     , LG = customer.list.sub$LG
                     , location = customer.list.sub$location
                     , line = customer.list.sub$line)
  return(customer.list.sub)
}

customer.location.line.productID <- function(customer, location, line, product_ID){
  product_ID <- data.frame(product_ID)
  product_ID <- product_ID[product_ID$customer == customer,]
  product_ID <- product_ID[product_ID$location == location,]
  if(nchar(unique(product_ID$line)[1]) != 0)   product_ID <- product_ID[product_ID$line == line,]
  product_ID <- product_ID[ ,c("beverage", "ID", "ID2", "ID3")]

  product_ID <- product_ID[ , c(T, !is.na(apply(product_ID[ , -1], 2, sum)))]
  message("List with product IDs in ", location, ", line ", line )
  return(product_ID)
}

customer.location.line.products <- function(customer, location, line, firstday, lastday, product_ID){

  suppressMessages(product_ID <- customer.location.line.productID(customer, location, line, product_ID))

  wd.o <- getwd()
  setwd(.service_backup_path(customer, location, line))
  line.product.date <- do.call(rbind, lapply(dir(pattern =  paste0(line,".csv")), read.csv2))
  line.product.date <- line.product.date[which(nchar(as.character(line.product.date$Date)) == 10),]
  line.product.date <- line.product.date[which(line.product.date$Date >= firstday & line.product.date$Date <= lastday) ,]
  message("The following products were produced in ", location, ", line ", line, " in the chosen timeframe:")
  line.product.date.ID <- sort(unique(line.product.date$Produkt_1))
  message(paste(line.product.date.ID, collapse = ", "))

  if(!is.vector(product_ID)){
  line.product.date.name <- list()
  for(i in 2:ncol(product_ID)) line.product.date.name[[i]] <- product_ID[product_ID[,i] %in% line.product.date.ID,1]
  message(paste(sort(unlist(line.product.date.name)), collapse = ", "))}
}
