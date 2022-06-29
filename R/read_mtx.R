mtx_add <- function(qxxmtx, coltoremove = c("X1", "X2"), sheet = "Zugabe_p"){

  qxxmtx <- lapply(qxxmtx, function( x ) openxlsx::read.xlsx( x , sheet = sheet))

  rangep <- 1 : length(coltoremove)

  qxxmtx <- lapply(qxxmtx, function(x) x[ , c(rangep, which( apply( x[ , -c(rangep) ], 2, function(y) sum(y, na.rm = T)) != 0) + max( rangep ))])

  if(length(qxxmtx) > 1){
    qxxmtx <- do.call(cbind, qxxmtx)
    lp = T} else{
      qxxmtx <- qxxmtx[[ 1 ]]
      lp = F
    }

  if(lp) qxxmtxcol <- qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)][ , 1:(ncol(qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)]) / 2)]
  if(!lp) qxxmtxcol <- qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)][ , 1:(ncol(qxxmtx[ , names( qxxmtx ) %in% c( coltoremove)]))]

  qxxmtx <- qxxmtx[ , !names( qxxmtx ) %in% c( coltoremove)]

  qxxmtx <- qxxmtx[ which( apply(qxxmtx, 1, function(x) sum( x, na.rm = T)) != 0) , ]

  parameter <- names( qxxmtx )

  qxxmtx <- cbind( qxxmtxcol[ 1:nrow(qxxmtx) , ], qxxmtx)

  qxxmtx <- list(qxxmtx, parameter)

  names(qxxmtx) <- c("QXXMTX", "parameter")

  return( qxxmtx )

}

mtx_folder <- function(dir, parameter){

  for(i in 1:length(parameter)){
    setwd(dir)
    dir.create(parameter[i], showWarnings = F)
    setwd( paste0( "./", parameter[i]))
  }

}

mtx_move_spc <- function( dir.source, dir.target, parameter, filter = "VAS", filteron = T, SL = "SL"){

  setwd(dir.source)
  spc.files <- dir( pattern = ".spc$")

  for(i in 1:length(parameter)){

    file.to.copy <- grep( paste0("_", parameter[i], "_"), spc.files, value = T)

    if(filteron) file.to.copy <- grep( paste0("_", filter, "_"), file.to.copy, value = T, invert = T)
    if(!filteron) file.to.copy <- grep( paste0("_", filter, "_"), file.to.copy, value = T, invert = F)

    file.to.copy.SL <- grep( paste0( "_", "SL", "_" ), file.to.copy, value = T)
    file.to.copy <- grep( paste0( "_", "SL", "_" ), file.to.copy, value = T, invert = T)

    setwd( dir.target )
    setwd( paste0("./", parameter[i]))

    if( length(file.to.copy) > 0)
      file.copy(from = paste0( dir.source, file.to.copy)
                , to = basename(file.to.copy)
                , overwrite = T
                , recursive = F)

    if( length(file.to.copy.SL) > 0)
      file.copy(from = paste0( dir.source, file.to.copy.SL)
                , to = basename(file.to.copy.SL)
                , overwrite = T
                , recursive = F)
  }

}

mtx_plot_spc <- function( dir
                          , beverage
                          , parameter = NA
                          , baseline = NA
                          , pngplot = F
                          , plotlyplot = T
                          , filestext = NA
                          , colp = NA
                          , subfiles = NA
                          , recursive = T
                          , write = T){

  # Plot and write spectra ####

  if( !is.na(parameter)) dir <- paste0( dir, "/", parameter)
  spc.files <- list.files(dir, pattern = ".spc$", recursive = recursive)

  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  if( length(spc.files) == 0 ){ stop_quietly() }

  filestext <- substr(basename( spc.files )
                      , unlist(gregexpr("_DT", basename( spc.files ), ignore.case = F)) + 8
                      , unlist(gregexpr("01_00", basename( spc.files ), ignore.case = F)) - 3)
  filestext <- filestext [which(nchar(filestext) > 0)]

  spc_daten <-  read_spc_files(directory = dir
                               , baseline = baseline
                               , pngplot = pngplot
                               , plotlyplot = plotlyplot
                               , recursive = recursive
                               , filestext = filestext
                               , colp = colp
                               , subfiles = subfiles)

  if(write == T) write_spc_files(spc_daten$au, write = T, filename = paste0(date(), "_", beverage, "_", "spc"), return_R = F)

  if(length(dir( pattern = paste0(date(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(date(), "_ref.html") ), paste0(date(), "_", beverage, "_", "ref.html"))
  if(length(dir( pattern = paste0(date(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(date(), "_drk.html") ), paste0(date(), "_", beverage, "_", "drk.html"))
  if(length(dir( pattern = paste0(date(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(date(), "_spc.html") ), paste0(date(), "_", beverage, "_", "spc.html"))
  if(length(dir( pattern = paste0(date(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(date(), "_trans.html") ), paste0(date(), "_", beverage, "_", "trans.html"))

  if(length(dir( pattern = paste0(date(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(date(), "_ref.csv") ), paste0(date(), "_", beverage, "_", "ref.csv"))
  if(length(dir( pattern = paste0(date(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(date(), "_drk.csv") ), paste0(date(), "_", beverage, "_", "drk.csv"))
  if(length(dir( pattern = paste0(date(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(date(), "_spc.csv") ), paste0(date(), "_", beverage, "_", "spc.csv"))
  if(length(dir( pattern = paste0(date(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(date(), "_trans.csv") ), paste0(date(), "_", beverage, "_", "trans.csv"))

}

