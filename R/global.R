source(paste0(wd$scripts,"_function_z_rm_NULL.R"))
source(paste0(wd$scripts,"_function_z_moveme.R"))
source(paste0(wd$scripts,"_function_plot_spc.R"))
source(paste0(wd$scripts,"_function_transfer_csv.R"))
source(paste0(wd$scripts,"_function_z_opendir.R"))
source(paste0(wd$scripts,"_function_z_breakfun.R"))
source(paste0(wd$scripts,"_function_z_xaxisdate.R"))
source(paste0(wd$scripts,"_function_z_head10.R"))
source(paste0(wd$scripts,"_function_z_clipboard.R"))
source(paste0(wd$scripts,"_function_z_bias.R"))
source(paste0(wd$scripts,"_function_z_remove_numeric_x.R"))
source(paste0(wd$scripts,"_function_z_date.R"))
source(paste0(wd$scripts,"_function_z_path_length_correction.R"))
source(paste0(wd$scripts,"_function_z_rm_NULL.R"))
source(paste0(wd$scripts,"_function_z_moveme.R"))
source(paste0(wd$scripts,"_function_z_matplotp.R"))
source(paste0(wd$scripts,"_function_z_lastfile_servicebackup.R"))
source(paste0(wd$scripts,"_function_z_normalize_to_wl.R"))
source(paste0(wd$scripts,"_function_z_txt_csv.R"))

.colp <- (c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
.colp <- c(.colp, "springgreen", "bisque3", "violetred1")

.parusr <- function(position, xx = .15, yy = .2){
  if(position == "topleft"){
    x <- par("usr")[1] - diff(par("usr")[1:2]) * xx
    y <- par("usr")[4] + diff(par("usr")[3:4]) * yy
  }
  return(list(x,y))
  
}

ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}


moving.average.time <- function(ma.dat, ma.time, ma.minutes = 10, ma.n = 5){
  
  ma.time.diff <- unique( c(1 , which( as.numeric( diff( ma.time ) ) > ma.minutes * 60 ), length(ma.time) ) )
  
  ma.time.diff.seq <- mapply( function( x,y ) x:y
                              , x = ma.time.diff[ - length(ma.time.diff) ]
                              , y = ma.time.diff[ - 1 ]  )
  
  
  ma.dat <- ma.dat <- unlist(lapply(ma.time.diff.seq, function(x) as.numeric(stats::filter( ma.dat[ x ], rep(1 / ifelse(length(ma.dat[ x ]) <= ma.n, length(ma.dat[ x ]), ma.n )
                                                                                                   , ifelse(length(ma.dat[ x ]) <= ma.n, length(ma.dat[ x ]), ma.n )), sides = 1))))
  
  
  return( ma.dat )
}

.lg3.status <- function(data){
  
  lg3.status <- c("diet", "brix", "co2", "conductivity", "FluidPressure", "FluidFlow", "FluidTemperature", "SpectrometerTemperature", "RackTemperature", "AmbientTemperature", "integrationTime", "accumulations")
  
  lg3.status <-  names(data)[ names(data) %in% lg3.status ]
  
  if( is.data.table(data) ) data <- data[ , names(data) %in% lg3.status, with = F]
  if( !is.data.table(data) ) data <- data[ , names(data) %in% lg3.status]
  
  suppressWarnings(  data <- data[ , which( apply(data, 2, function(x) !any(x)) == F), with = F] )
  
  lg3.status <- names(data)
  
  data.col <- ncol(data)
  
  if(data.col == 1) par(mfrow = c(1,1))
  if(data.col == 2) par(mfrow = c(1,1))
  if(data.col == 3) par(mfrow = c(1,3))
  if(data.col == 4) par(mfrow = c(2,2))
  if(data.col > 4) par(mfrow = c(2,3))
  if(data.col > 6) par(mfrow = c(3,3))
  if(data.col > 9) par(mfrow = c(3,4))
  
  returnlist <- list(data, lg3.status)
  names(returnlist) <- c("data", "names")
  return(returnlist)
}
