date <- function()  substr(gsub("-","",Sys.Date()),3,8)

time <- function(){
  datetime <- Sys.time()
  timep <- strftime(as.character(Sys.time()), format = "%H:%M:%S")
  timep_frame <- unlist(gregexpr(":", timep))

  timep <- paste0(substr(timep, 1 , (timep_frame[1] - 1))
                  , substr(timep, (timep_frame[1] + 1) , (timep_frame[2] - 1))
                  , substr(timep, (timep_frame[2] + 1) , nchar(timep)))
}
datetime <- function() paste(.date(), .time(), sep = "_")

