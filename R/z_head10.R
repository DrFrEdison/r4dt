.head10 <- function(data, nrowp = 10, ncolp = 10)  data[1:nrowp,1:ncolp]
.tail10 <- function(data, nrowp = 10, ncolp = 10)  data[(nrow(data) - (nrowp-1)):nrow(data),1:ncolp]

