pls.model.customer <- function(customer, beverage, LG, parameter, alpha = .05, gamma = 0.01, raw.spc, raw.numcol){

  # set wd ####
  originalwd <- getwd()
  setwd(wd$model[[1]])

  # read model overview
  model.overview <-  read_ods("dt_model_overview.ods")

  # Coffein!
  if(parameter == "Koffein") parameter <- "Coffein"

  # Find model parameter
  model.para <- model.overview[model.overview$customer==customer & model.overview$beverage==beverage & model.overview$LG==LG & model.overview$substance==parameter,][1,]

  # Find model csv ####
  setwd(paste0(setwd(unlist(wd$model)[1]),"/",model.para$customer))

  model.filename <- grep(parameter,dir(),value = T)
  if(LG != "SG"){ model.filename <- grep(paste0("LG",LG),model.filename,value=T) } else{ grep(paste0(LG),model.filename,value=T) }
  model.filename <- grep(beverage,model.filename,value=T)
  model.filename <- grep(customer,model.filename,value=T)
  model.filename <- model.filename[1]

  model.csv <- fread(model.filename,dec=",")

  # model subset?
  if(!is.na(model.para$subset)) model.csv <- model.csv[eval(parse(text=paste0("c(",gsub("-",":",as.character(model.para$subset)),")"))),]

  # column of y ####
  model.col.y <- which(names(model.csv)==parameter)
  names(model.csv)[model.col.y] <- parameter

  # transfer function for model ####
  model.trs <- transfer_csv(model.csv,
                             p = ifelse(is.na(model.para$p), 2, model.para$p),
                             n1 = ifelse(is.na(model.para$n1), 7, model.para$n1),
                             n2 = ifelse(is.na(model.para$n2), 11, model.para$n2))

  model.wl <- sort(unique(c(model.para$wl1:model.para$wl2, model.para$wl3, model.para$wl4)))

  model.pls <- mdatools::pls(model.trs[[  grep(model.para$spc, names(model.trs))[1] ]][ , colnames(model.trs[[  grep(model.para$spc, names(model.trs))[1] ]]) %in% paste0("X", model.wl)]
                             , model.trs$data[ , model.col.y]
                             , ncomp = model.para$ncomp
                             , cv = 5
                             , alpha = alpha
                             , gamma = gamma)

  raw.spc <- raw.spc[ , raw.numcol$numcol, with = F]

  if(model.para$spc == "1st"){
    raw.spc <- raw.spc[ , savitzkyGolay(.SD
                                        , m = 1
                                        , p = ifelse(is.na(model.para$p), 2, model.para$p)
                                        , w = ifelse(is.na(model.para$n1), 7, model.para$n1))]
    raw.spc <- data.table(raw.spc)
  }

  if(model.para$spc == "2nd"){
    raw.spc <- raw.spc[ , savitzkyGolay(.SD
                                        , m = 2
                                        , p = ifelse(is.na(model.para$p), 2, model.para$p)
                                        , w = ifelse(is.na(model.para$n2), 11, model.para$n2))]
    raw.spc <- data.table(raw.spc)
  }


  raw.spc <- raw.spc[ , colnames(raw.spc) %in% paste0("X", model.wl), with = F]

  pred.pls <- predict(model.pls
                      , raw.spc
                      , ncomp = model.para$ncomp)

  model <- list()
  model$model <- model.pls
  model$predpara <- pred.pls$xdecomp
  model$pred <- as.numeric(pred.pls$y.pred[ , model.para$ncomp, ])
  model$csv <- model.csv
  model$name <- model.filename
  model$para <- model.para
  if(is.na(model$para$wl3)) model$wl <- c(model$para$wl1 : model$para$wl2)
  if(!is.na(model$para$wl3)) model$wl <- c(model$para$wl1 : model$para$wl2, model$para$wl3 : model$para$wl4)

  model$val$model <- data.frame(Q = model$model$Qlim[1,model.para$ncomp], T2 = model$model$T2lim[1,model.para$ncomp])
  model$val$val <- data.frame(Q = model$predpara$Q[,model.para$ncomp], T2 = model$predpara$T2[,model.para$ncomp])

  return(model)
}
