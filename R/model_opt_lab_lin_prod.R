model_opt_lab_lin_prod_match <- function(lab_date, lab_para
                                         , prod_data
                                         , LG3_time, LG3_timerange
                                         , unit, directory
                                         , lin_csv
                                         , pls_model
                                         , pls_substance
                                         , pls_wl
                                         , pls_ncomp){

  opt <- list()
  match_date <- list()
  match_subdat <- list()
  match_subdatx <- list()
  match_labmedian <- list()
  subdat <- list()

  for(i in 1:length(unit)){

    if(substr(prod_data[[i]]$datetime, 11,11)[1] != " ")  prod_data[[i]]$datetime <- as.POSIXct(strptime(prod_data[[i]]$datetime, "%Y-%m-%dT%H:%M:%S") )

    if(LG3_time[[i]] >= LG3_timerange[[i]])
      match_subdat[[i]] <- lapply(lab_date[[i]], function(x) which( prod_data[[i]]$datetime >= (x - LG3_time[[i]] - LG3_timerange[[i]] ) &
                                                                      prod_data[[i]]$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))
    if(LG3_time[[i]] < LG3_timerange[[i]])
      match_subdat[[i]] <- lapply(lab_date[[i]], function(x) which( prod_data[[i]]$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))

    lab_date[[i]] <- lab_date[[i]][which(lapply(match_subdat[[i]], length) != 0)]
    lab_para[[i]] <- lab_para[[i]][which(lapply(match_subdat[[i]], length) != 0)]

    subdat[[i]] <- prod_data[[i]][sort(unique(unlist(match_subdat[[i]]))) , ]
    subdat[[i]] <- .transfer_csv(csv.file = subdat[[i]])

    if(LG3_time[[i]] >= LG3_timerange[[i]])
      match_subdatx[[i]] <- lapply(lab_date[[i]], function(x) which( subdat[[i]]$data$datetime >= (x - LG3_time[[i]] - LG3_timerange[[i]] ) &
                                                                       subdat[[i]]$data$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))

    if(LG3_time[[i]] < LG3_timerange[[i]])
      match_subdatx[[i]] <- lapply(lab_date[[i]], function(x) which( subdat[[i]]$data$datetime <= (x - LG3_time[[i]] + LG3_timerange[[i]] )))

    match_labmedian[[i]]<- median(lab_para[[i]])

    return(list(lab_date, lab_para, subdat, match_subdatx, match_labmedian))
  }
}

model_opt_lab_lin_prod_calc <- function(lab_date, lab_para
                                        , prod_data
                                        , LG3_time, LG3_timerange
                                        , unit, directory
                                        , lin_csv
                                        , lin_dat
                                        , pls_model
                                        , pls_substance
                                        , pls_wl
                                        , pls_ncomp
                                        , matchfun){

  opt <- list()
  match_date <- list()
  match_subdat <- list()
  match_subdatx <- list()
  match_labmedian <- list()
  subdat <- list()

  lab_date = matchfun[[1]]
  lab_para = matchfun[[2]]
  subdat = matchfun[[3]]
  match_subdatx = matchfun[[4]]
  match_labmedian = matchfun[[5]]


  plslist <- list()
  pred_lin <- list()
  for(z in 1:length(pls_model)){
    plslist <- pls_function(csv_transfered = pls_model[[z]]
                            , substance = pls_substance
                            , wlr = pls_wl
                            , ncomp = pls_ncomp)

    pred_lin <- produktion_prediction(csv_transfered = lin_csv
                                      , pls_function_obj = plslist
                                      , ncomp = pls_ncomp
                                      , messaget = F)

    pred_subdat <- list()
    match_min <- list()
    match_sd <- list()
    match_sd_mod <- list()

    for(i in 1:length(unit)){

      pred_subdat[[i]] <- produktion_prediction(csv_transfered = subdat[[i]], pls_function_obj = plslist, ncomp = pls_ncomp, messaget = F)
      match_min[[i]] <- lapply(pred_subdat[[i]]$prediction, function(b) lapply(b, function(a) mapply(function(x, z) apply(a, 2, function(y) y[ x ][ which.min( abs ( y[ x ] - z ))])
                                                                                                     , x = match_subdatx[[i]]
                                                                                                     , z = lab_para[[i]])))
      match_sd[[i]] <- lapply(match_min[[i]], function(a) lapply(a, function(b) apply(b, 1, function(c) sd( lab_para[[i]] - (c - bias( median( c ), 0, match_labmedian[[i]], 3))))))

      match_sd_mod[[i]] <- list()

      for(k in 1:length(match_sd[[i]])){
        if(k == 1) spc = "spc"
        if(k == 2) spc = "1st"
        if(k == 3) spc = "2nd"

        match_sd_mod[[i]][[k]] <- list()

        for(j in 1:length(match_sd[[i]][[k]])){
          match_sd_mod[[i]][[k]][[j]] <- list()
          match_sd_mod[[i]][[k]][[j]] <- data.frame(spc = spc, ncomp = j
                                                    , wl1 = substr(names(match_sd[[i]][[k]][[j]]), 1, 3)
                                                    , wl2 = substr(names(match_sd[[i]][[k]][[j]]), 5, 7)
                                                    , wl3 = substr(names(match_sd[[i]][[k]][[j]]), 9, 11)
                                                    , wl4 = substr(names(match_sd[[i]][[k]][[j]]), 13, 15)
                                                    , sd = round(match_sd[[i]][[k]][[j]], 2))
        }
      }
      match_sd_mod[[i]] <- do.call(rbind, lapply(match_sd_mod[[i]], function(x) do.call(rbind, x)))
      names(match_sd_mod[[i]])[grep("sd", names(match_sd_mod[[i]]))] <- paste0("sd.", unit[[i]])
    }

    # R2 und lin range ####
    lin_minmax <- lapply(pred_lin$prediction, function(x) lapply(x, function(y) apply(y, 2, function(z) z[length(z)] - z[1])))
    lin_R2 <- lapply(pred_lin$prediction, function(x) lapply(x, function(y) apply(y, 2, function(z) cor(z, lin_dat) ^ 2)))

    lin_sd <- lapply(pred_lin$prediction, function(x) lapply(x, function(y) apply(y, 2, function(z) sd ( lin_dat - (z - median(z) )))))

    # merge ####
    if(length(unit) > 1){
      addcol <- lapply(match_sd_mod, function(x) x[ , ncol(x)])
      addcol <- addcol[[-1]]
      match_merge <- cbind(match_sd_mod[[1]], cbind(addcol))

      names(match_merge)[8:length(match_merge)] <- paste0("sd.", unlist(unit[-1]))
    } else { match_merge <- match_sd_mod[[1]]}

    match_merge <- cbind(match_merge, lin_range = unlist(lin_minmax), R2 = unlist(lin_R2), lin_sd = unlist(lin_sd))
    match_merge <- data.frame(match_merge)

    if(length(grep("sd", names(match_merge))) > 1) match_merge$sd.sum <- apply(match_merge[ , grep("sd", names(match_merge))],1,function(x) sum(x, na.rm =T))
    if(length(grep("sd", names(match_merge))) > 1) match_merge <- match_merge[order(match_merge$sd.sum) , ]
    if(length(grep("sd", names(match_merge))) == 1) match_merge <- match_merge[order(match_merge[ , grep("sd", names(match_merge))] ) , ]

    setwd(directory)
    write.csv2(match_merge, paste0(kk <- datetime(), "_analysis_", formatC(z, width = 2, format = "d", flag = "0"), ".csv"), row.names = F)
    if(z == 1) kk0 <- kk
    if(z == 1) kk0 <- as.numeric(gsub("_","",substr(kk0, 1, 13)))

    write.csv2(data.frame(cbind(pls_model$date, pls_model$spc)), paste0(kk, "_modelmatrix_", formatC(z, width = 2, format = "d", flag = "0"), ".csv"), row.names = F)
    gc()
    message(paste(z, "finished at",Sys.time()))
  }

  setwd(directory)
  export_files <- dir()[which(as.numeric(gsub("_","",substr(dir(), 1, 13))) >= kk0)]
  export_files <- export_files[grep("_analysis", export_files)]

  export_ID <- as.numeric(substr(export_files, 24, 25))

  export_raw <- lapply(export_files, read.csv2)

  export_raw <- mapply(function(x, y) cbind(x, matrix = as.numeric(y))
                       , x = export_raw
                       , y = as.list(export_ID)
                       , SIMPLIFY = F)

  export_raw <- do.call(rbind, export_raw)
  export_raw <- export_raw[ order(export_raw[ , max(grep("sd", names(export_raw)))]) , ]

  write.csv2(export_raw, paste0(datetime(), "_analysis_merge.csv"), row.names = F)
  file.remove(export_files)

  gc()

  return(export_raw)

}
