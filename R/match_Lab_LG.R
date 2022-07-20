#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###                         ###
###  ######       ########  ###
###  ##    ##     ########  ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##     ##       ##     ###
###  ##    ##        ##     ###
###  ######          ##     ###
###                         ###
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###                         ###
### By Markus Kurtz         ###
### For Dausch Technologies ###
### 2022                    ###
###                         ###
#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
###
###
### Script to match Lab data with LG data according to time stamps
### Each location and unit has its own time.diff found in SP-201
###
### Script return a list with 4 objects:
### [[1]] data.frame with Lab vs LG values, LG values bias corrected and time difference between Lab and LG date times
### [[2]] model bias according to validation table
### [[3]] sd between Lab and LG values according to validation table
### [[4]] validation table with incorporated Lab data, also non matching Lab and LG data

### Abbreviations: LG = LiquiGuard

### Function requires the package plyr

### Version 01
### Date = 2022-02-02

match.Lab.LG <- function(daytime.Lab # Vector of Lab day and time in a POSIXct format. Timezone must be specified by tizo, standard is "UTC (see last function term)
                         , daytime.LG # Vector of LG day and time in a POSIXct format. Timezone must be specified by tizo, standard is "UTC" (see last function term)
                         , value.Lab # Vector of Lab values
                         , value.LG # Vector of LG values
                         , time.diff = NA # allowed time difference in seconds between LG datetime and Lab datetime according to SP-201
                         , time.range = NA # allowed time range in seconds around time.diff according to SP-201
                         , tizo = "UTC"
                         , mode = c("value", "time")){ # mode of error calculation; value for minimal value, time for minimal time deviation

  if(any(is.na(daytime.Lab))) stop("No daytime.Lab defined") # stop function when no daytime.Lab is defined
  if(all(is.na(daytime.LG))) stop("No daytime.LG defined") # stop function when no daytime.LG is defined
  if(any(is.na(value.Lab))) stop("No value.Lab defined") # stop function when no value.Lab is defined
  if(all(is.na(value.LG))) stop("No LG Prediction defined") # stop function when no value.LG is defined
  if(is.na(time.diff)) stop("No time.diff defined") # stop function when no time.diff is defined
  if(is.na(time.range)) stop("No time.range defined") # stop function when no time.range is defined

  if(length(daytime.Lab) != length(value.Lab)) stop("Lab values and Lab times have a different lenght") # stop function when Lab values and Lab times have different lengths
  if(length(daytime.LG) != length(value.LG)) stop("LG values and LG times have a different lenght") # stop function when LG values and LG times have different lengths

  if(all(tz(daytime.Lab) != "", tz(daytime.Lab) != "UTC")) warning("Time zone of Lab data seems to be not UTC, please check!") # tz(x) creates an empty string "" when it equals the current time zone
  if(all(tz(daytime.LG) != "" , tz(daytime.LG) != "UTC")) warning("Time zone of Lab data seems to be not UTC, please check!") # tz(x) creates an empty string "" when it equals the current time zone

  if(length(which(duplicated(daytime.Lab))) > 0) warning("Duplicated times in Lab data") # Warns when duplicated are in Lab data
  if(length(which(duplicated(daytime.LG))) > 0) warning("Duplicated times in LG data") # Warns when duplicated are in LG data

  # Order values and time by time
  value.Lab <- value.Lab[order(daytime.Lab)]
  daytime.Lab <- daytime.Lab[order(daytime.Lab)]

  value.LG <- value.LG[order(daytime.LG)]
  daytime.LG <- daytime.LG[order(daytime.LG)]

  # Create empty list
  dat <- list()

  # Search time LG-timestamps to match the defined time according to SP-201
  if(time.diff >= time.range)
    dat$match.time.LG <- lapply(daytime.Lab, function(x) daytime.LG[which( daytime.LG >= x - time.diff - time.range &
                                                                             daytime.LG <= x -time.diff + time.range)])

  # For cases when time.diff < time.range the formula must be adapted to avoid time differences < 0 (i.e. lab value previous to LG values what is impossible)
  if(time.diff < time.range)
    dat$match.time.LG <- lapply(daytime.Lab, function(x) daytime.LG[which( daytime.LG <= x -time.diff + time.range)])

  if(mode == "time")
  # In the matched timeframe, which time has the shortest time distance to the Lab value?
  # Calculated by which absolute Lab.time minus LG.time - allowed time difference according to SP-201 is smallest?
  dat$match.time.LG.min <-  mapply(function(x, y) y[ which.min( abs( x - y - time.diff ))]
                                   , x = daytime.Lab
                                   , y = dat$match.time.LG
                                   , SIMPLIFY = F)

  if(mode == "value")
  # In the matched timeframe, which LG value has the minimal deviation compared to the Lab value?
  # Calculated by which absolute value.LG - value.Lab Lab.time minus LG.time - allowed time difference according to SP-201 is smallest?
  dat$match.time.LG.min <-  mapply(function(x , y) y[ which.min( abs( value.LG[ which(as.character( daytime.LG ) %in% as.character( y )) ] - x ) ) ]
                                   , y = dat$match.time.LG
                                   , x = value.Lab
                                   , SIMPLIFY = F)

  dat$match.LG.value <- lapply(dat$match.time.LG.min, function(x) value.LG[ which(daytime.LG %in% x) ])

  # if a LG timestamp is duplicated
  dat$match.LG.value <- lapply(dat$match.LG.value, median)

  # mark unmatching rows with NA
  dat$match.time.LG.min[ which ( unlist(  lapply(dat$match.time.LG.min, length) ) == 0) ] <- NA
  dat$match.LG.value[ which ( unlist(  lapply(dat$match.LG.value, length) ) == 0) ] <- NA

  # Create LG and lab data frame
  dat$frame.LG.lab <-  data.frame(LG.time = unlist( lapply(dat$match.time.LG.min, as.character))
                                  , Lab.time = daytime.Lab
                                  , LG.value = unlist( dat$match.LG.value )
                                  , Lab.value = value.Lab)

  dat$frame.LG.lab$difftime <- as.numeric(as.POSIXct(dat$frame.LG.lab$Lab.time, tz = tizo) - as.POSIXct(dat$frame.LG.lab$LG.time, tz = tizo))

  dat$frame.LG.lab <- dat$frame.LG.lab[ dat$frame.LG.lab$difftime <= time.diff + time.range & dat$frame.LG.lab$difftime >= time.diff - time.range , ]
  dat$frame.LG.lab$Lab.time <- daytime.Lab
  dat$frame.LG.lab$Lab.value <- value.Lab

  # dat$frame.LG.lab <- dat$frame.LG.lab[ !is.na( dat$frame.LG.lab$LG.time ) , ]

  # create export data for validation table according to FO-223
  dat$frame.val <- data.frame(NR = 1:length(daytime.LG)
                              , Datetime = daytime.LG
                              , Datum = as.Date(daytime.LG, tz = tizo)
                              , Uhrzeit = strftime(daytime.LG, format = "%H:%M:%S", tz = tizo)
                              , LG = value.LG
                              , Lab = NA)

  # match LG time with matching time. ugly slow, but is save for duplicates
  dat$frame.valvec <- lapply(dat$frame.LG.lab$LG.time, function(x) which(as.character(dat$frame.val$Datetime) %in% x))

  # match lab data to validation table
  dat$frame.val$Lab[ unlist(dat$frame.valvec) ] <- dat$frame.LG.lab$Lab.value[ !is.na(dat$frame.LG.lab$LG.value)]

  # calculate bias from validation table
  dat$bias <- median(dat$frame.val$LG[ which(!is.na(dat$frame.val$Lab)) ]) - median(dat$frame.val$Lab[ which(!is.na(dat$frame.val$Lab)) ])
  if(is.na(dat$bias)) stop("Bias calculation produces NA")

  # correct LG values with bias
  dat$frame.LG.lab$LG.value.c <- dat$frame.LG.lab$LG.value - dat$bias
  dat$frame.val$LG <- dat$frame.val$LG - dat$bias

  # Difference between Lab and LG
  dat$frame.LG.lab$diff.Lab.LG <- dat$frame.LG.lab$Lab.value - dat$frame.LG.lab$LG.value.c

  # sd between difference between Lab and LG
  dat$sd.Lab.LG <- sd(sort(dat$frame.val$Lab - dat$frame.val$LG))

  # include missing LG data into val table
  dat$frame.missing.LG <- dat$frame.LG.lab[is.na(dat$frame.LG.lab$LG.time) , c("Lab.time", "LG.value", "Lab.value")]

  # shape dat$frame.missing.LG with same columns as dat.frame.val (otherwise rbind.fill won't work)
  dat$frame.missing.LG <- data.frame(Datetime = dat$frame.missing.LG$Lab.time
                                     , Datum = as.Date(dat$frame.missing.LG$Lab.time, tz = tizo)
                                     , Uhrzeit = strftime(dat$frame.missing.LG$Lab.time, format = "%H:%M:%S", tz = tizo)
                                     , LG = dat$frame.missing.LG$LG.value
                                     , Lab = dat$frame.missing.LG$Lab.value)

  dat$frame.val <- rbind.fill(dat$frame.val, dat$frame.missing.LG) # rbind.fill using plyr
  dat$frame.val <- dat$frame.val[order(dat$frame.val$Datetime) , ] # order by date
  dat$frame.val <- dat$frame.val[ - which( is.na( dat$frame.val$LG ) & is.na( dat$frame.val$Lab ) ) , ]
  dat$frame.val <- cbind(dat$frame.val, package = NA)
  dat$frame.val <- dat$frame.val[ , moveme( colnames( dat$frame.val ), "NR Datum Uhrzeit package first")]

  dat$frame.val$NR <- 1:nrow(dat$frame.val) # new NR column

  # final shape of return data
  dat$frame.LG.lab[ , 3:ncol(dat$frame.LG.lab)] <- apply(dat$frame.LG.lab[ , 3:ncol(dat$frame.LG.lab)], 2, function(x) round(x, 3)) # round by 3 digits
  dat$sd.Lab.LG <- round(dat$sd.Lab.LG, 3) # calculate sd between LG and lab
  dat$frame.val$Datetime <- NULL # remove Datetime columns in validation table
  dat$frame.val$LG <- round(dat$frame.val$LG, 3)
  dat$bias <- round(dat$bias, 3)
  dat$sd <- round(dat$sd, 2)
  # export as list
  dat$returnlist <- list(dat$frame.LG.lab, dat$bias, dat$sd.Lab.LG, dat$frame.val)

  names(dat$returnlist) <- c("data", "bias", "sd", "val")

  return(dat$returnlist)
}

