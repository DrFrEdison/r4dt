use_model_on_device1 <- function(customer, beverage, LG, parameter, csv_transfered, return_type = "prediction"){

  originalwd <- getwd()
  require(readODS)
  setwd(unlist(r4dt::wd$data)[1])
  model_overview <-  read_ods("dt_model_overview.ods")

  if(parameter == "Coffein") parameter <- "offein"
  if(parameter == "Koffein") parameter <- "offein"

  rowp <- which(model_overview$customer %in% customer & model_overview$beverage %in% beverage & model_overview$LG %in% LG)
  rowp <- rowp[ rowp %in% grep(parameter, model_overview$substance) ]

  model_overviewp <- model_overview[rowp,]

  if(nrow(model_overviewp)) model_overviewp <- model_overviewp[1,]

  setwd(unlist(wd$model)[1])
    setwd(paste0(setwd(unlist(wd$model)[1]),"/",model_overviewp$customer))

  txtlist <- grep(parameter,dir(),value = T)
  if(LG != "SG"){ txtlist <- grep(paste0("LG",LG),txtlist,value=T) } else{ grep(paste0(LG),txtlist,value=T) }
  txtlist <- grep(beverage,txtlist,value=T)
  txtlist <- grep(customer,txtlist,value=T)

  txtlist <- txtlist[1]

  dat <- fread(txtlist,dec=",")

  # if(length(grep("offein",parameter))>0){
  #   if(length(grep(parameter,names(dat)))==0) colnump <- names(dat)[grep(gsub("C","K",parameter),names(dat))]  else colnump <- names(dat)[grep(parameter,names(dat))]} else colnump <- names(dat)[grep(parameter,names(dat))]


  colnumpp <- grep(parameter, names(dat))
  setDF(dat)

  if(!is.na(model_overviewp$subset)) dat <- dat[eval(parse(text=paste0("c(",gsub("-",":",as.character(model_overviewp$subset)),")"))),]

  dat <- dat[,!is.na(colnumpp)]
  names(dat)[colnumpp] <- parameter
  names(dat)[1] <- "ID"

  dat <- transfer_csv(dat,
                       p = ifelse(is.na(model_overviewp$n1) | model_overviewp$n1 == 0, 2, model_overviewp$p),
                       n1 = ifelse(is.na(model_overviewp$n1) | model_overviewp$n1 == 0, 7, model_overviewp$n1),
                       n2 = ifelse(is.na(model_overviewp$n1) | model_overviewp$n1 == 0, 11, model_overviewp$n2))
  matrixtochoose <- NA
  whichnot <- NA
  substance <- parameter
  wlr <- data.frame(wl1=model_overviewp$wl1, wl2=model_overviewp$wl2)
  if(!is.na(model_overviewp$wl3))   wlr <- data.frame(wl1=model_overviewp$wl1,wl2=model_overviewp$wl2,wl3=model_overviewp$wl3,wl42=model_overviewp$wl4)

  ncomp <- model_overviewp$ncomp
  derivative <- as.character(model_overviewp$spc)
  namecolumn <- "ID"

  pls_function_obj <- pls_function(csv_transfered = dat, substance = substance, wlr = wlr,ncomp = ncomp, spc = derivative)

  if(return_type == "prediction"){


  setwd(setwd(unlist(wd$model)[1]))
  setwd(originalwd)

  suppressMessages(prediction <- produktion_prediction(csv_transfered = csv_transfered, pls_function_obj = pls_function_obj, ncomp = ncomp))

  prediction_final <- pred_of_model(prediction, model_overviewp$wl1, model_overviewp$wl2, model_overviewp$wl3, model_overviewp$wl4, ncomp, derivative)

  message(paste("Parameter =", model_overviewp$substance
                , "wl1 =", model_overviewp$wl1
                , "wl2 =", model_overviewp$wl2
                , "wl3 =", model_overviewp$wl3
                , "wl4 =", model_overviewp$wl4
                , "PC =", model_overviewp$ncomp
                , "derivative =", model_overviewp$spc))
  }
  setwd(originalwd)
  if(return_type == "prediction") return(prediction_final)
  if(return_type == "model"){

    pls_lm <- pls_lm_function(pls_function_obj = pls_function_obj, csv_transfered = dat, wlr = wlr, ncomp = ncomp)
    pls_lm <- pls_lm[ nrow(pls_lm) , ]

    model <- list()
    model$model <- pls_function_obj$pls[[1]]
    model$csv <- dat
    model$name <- txtlist
    model$para <- model_overviewp
    model$lm <- pls_lm
    if(is.na(model$para$wl3)) model$wl <- c(model$para$wl1 : model$para$wl2)
    if(!is.na(model$para$wl3)) model$wl <- c(model$para$wl1 : model$para$wl2, model$para$wl3 : model$para$wl4)

  return(model)}
}

