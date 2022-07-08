median_spc <- function(spc, numcol = NA){

  if( is.na(numcol[1])) suppressWarnings( median_spc <- as.numeric(apply(spc, 2, function(x) median(x, na.rm = T))))

  if(!is.na(numcol)[1]) suppressWarnings( median_spc <- as.numeric( apply( spc[ , numcol$numcol, with = F], 2, function(x) median(x, na.rm = T))))

  return(median_spc)
}

median_daily_spc <- function(spc, date, tz = "Europe/Berlin", numcol = NA) {
  date <- as.Date(date, tz = tz)
  date.u <- unique(as.Date(date, tz = tz))
  date.us <- format(as.Date(date.u), "%y%m%d")
  date.usx <- paste0("X", date.us)
  names(date.u) <- date.usx
  date.v <- lapply(date.u, function(x) which(x == date))

  if( is.na(numcol[1])) suppressWarnings( daily_median <- lapply(date.v, function(x) median_spc(spc[ x , ])))

  if( !is.na(numcol[1])) suppressWarnings( daily_median <- lapply(date.v, function(x) median_spc(spc = spc[ x , ], numcol = numcol)))

  return(daily_median)
}

median_weekly_spc <- function (spc, date, tz = "Europe/Berlin", numcol = NA)
{
  cw_year <- paste0( strftime( date, format = "%y"), "_",   strftime( date, format = "%V"))

  cw_year.u <- unique(cw_year)

  cw_year.ux <- paste0("X", cw_year.u)
  names(cw_year.u) <- cw_year.ux

  cw_year.v <- lapply(date.u, function(x) which(x == cw_year))

  if (is.na(numcol[1]))
    suppressWarnings(weekly_median <- lapply(cw_year.v, function(x) median_spc(spc[x,
    ])))
  if (!is.na(numcol[1]))
    suppressWarnings(weekly_median <- lapply(cw_year.v, function(x) median_spc(spc = spc[x,
    ], numcol = numcol)))
  return(weekly_median)
}

fingerprint <- function(spc_0, spc_1, numcol = NA){

  if( is.na(numcol[1])) suppressWarnings( fp <- data.frame(t(apply(spc_1, 1, function(x) as.numeric(log10( spc_0 / x))))))

  if( !is.na(numcol[1])) suppressWarnings( fp <- data.frame(t(apply(spc_1[ , numcol$numcol, with = F], 1, function(x) as.numeric(log10( spc_0 / x))))) )

  if( is.na(numcol[1])) colnames(fp) <- colnames(spc_1)
  if( !is.na(numcol[1])) colnames(fp) <- colnames(spc_1)[ numcol$numcol ]
  return(fp)
}


