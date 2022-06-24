read.csv.LG <- function(firstday
                        , lastday
                        , customer = NA
                        , location = NA
                        , line
                        , product = NA
                        , typecode = NA
                        , Ringkessel = T
                        , typeof=c("drk","ref","spc")
                        , slim = T
                        , return.R = F
                        , product_ID = dt_customer_product_ID
                        , customer.list = dt_customer
                        , export_directory="C://csvtemp"){

  rowp <- which( customer.list$customer == customer & customer.list$location == location & customer.list$line == line )
  LG <- customer.list[ rowp , "LG"]

  if(LG == "LG 3" | LG == "3")
    dat <- read.csv.LG3(firstday = firstday
                        , lastday = lastday
                        , customer = customer
                        , location = location
                        , line = line
                        , product = product
                        , typecode = typecode
                        , typeof = typeof
                        , slim = slim
                        , return.R = return.R
                        , product_ID = product_ID
                        , export_directory = export_directory)

  if(LG == "LG 2" | LG == "2")
    dat <- read.csv.LG2(firstday = firstday
                        , lastday = lastday
                        , customer = customer
                        , location = location
                        , line = line
                        , product = product
                        , Ringkessel = Ringkessel
                        , typeof = typeof
                        , slim = slim
                        , return.R = return.R
                        , product_ID = product_ID
                        , export_directory = export_directory)

  if(LG == "LG 1" | LG == "1")
    dat <- read.csv.LG2(firstday = firstday
                        , lastday = lastday
                        , customer = customer
                        , location = location
                        , line = line
                        , product = product
                        , Ringkessel = Ringkessel
                        , typeof = typeof
                        , slim = slim
                        , return.R = return.R
                        , product_ID = product_ID
                        , export_directory = export_directory)

  if(LG == "SG")
    dat <- read.csv.SG(firstday = firstday
                       , lastday = lastday
                       , customer = customer
                       , location = location
                       , line = line
                       , product = product
                       , typeof = typeof
                       , export_directory = export_directory
                       , fastplot = F)

  if(return.R) return(dat)
}
