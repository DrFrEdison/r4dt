dir(wd$data)
dt_customer <- fread( paste0(wd$data, "/dt_customer.csv"))
dt_customer_product_ID <- fread( paste0(wd$data, "/dt_customer_product_ID.csv"))

setwd("D:/R_Git/r4dt/data")
save(dt_customer, file =  "dt_customer.rda")
save(dt_customer_product_ID, file =  "dt_customer_product_ID.rda")
