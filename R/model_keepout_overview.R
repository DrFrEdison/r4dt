keep.out.unsb.model <- function(customer, beverage, LG, parameter){

  originalwd <- getwd()
  require(readODS)
  setwd(unlist(wd$data)[1])
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
  dat <- transfer_csv(dat)

  dat.keep.out <- keep.out.unsb(model = dat, wl1 = model_overviewp$wl1, wl2= model_overviewp$wl2, wl3 = model_overviewp$wl3, wl4 = model_overviewp$wl4)
  dat.para <- data.frame(customer = model_overviewp$customer, beverage = model_overviewp$beverage, parameter = model_overviewp$substance, ncomp = model_overviewp$ncomp, spc = model_overviewp$spc
                         , wl1 = model_overviewp$wl1, wl2= model_overviewp$wl2, wl3 = model_overviewp$wl3, wl4 = model_overviewp$wl4)

  return(list(dat.keep.out, dat.para))
}
