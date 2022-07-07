model_parameter_write <- function(customer, location = NA, line = NA
                        , beverage, substance, LG
                        , ncomp, wl1, wl2, wl3, wl4, spc){

  # write to model data
  setwd(wd$data)
  dt$model.overview <- read_ods("dt_model_overview.ods")

  if(length(which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance)) == 0){
    dt$model.overview <- rbind(dt$model.overview
                               , data.frame(customer = customer
                                            , location = location
                                            , line = line
                                            , beverage = beverage
                                            , LG = dt_customer[ dt_customer$location == as.character(dt$para$location) , "LG"][1]
                                            , substance = substance
                                            , wl1 = wl1
                                            , wl2 = wl2
                                            , wl3 = wl3
                                            , wl4 = wl4
                                            , ncomp = ncomp
                                            , spc = spc
                                            , p = NA, n1 = NA, n2 = NA, seg = NA, Slope = NA, subset = NA)
    )
  }

  if(length(which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance)) == 1){

    dt$model.overview[which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance) , "wl1"] <- wl1
    dt$model.overview[which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance) , "wl2"] <- wl2
    dt$model.overview[which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance) , "wl3"] <- wl3
    dt$model.overview[which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance) , "wl4"] <- wl4
    dt$model.overview[which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance) , "ncomp"] <- ncomp
    dt$model.overview[which(dt$model.overview$beverage == beverage & dt$model.overview$substance == substance) , "spc"] <- spc
  }

  dt$model.overview <- dt$model.overview[ order(dt$model.overview$customer, dt$model.overview$beverage, dt$model.overview$substance),]
  write_ods(x = dt$model.overview, path = "dt_model_overview.ods", overwrite = T)

  setwd("./model")
  setwd(paste0("./", customer))

  fwrite(x = cbind(dt$model.raw$data, dt$model.raw$spc)
         , file = paste0( paste(customer
                                , beverage
                                , substance
                                , paste0("LG", as.character(dt_customer[ dt_customer$location == as.character(dt$para$location), "LG"][1]))
                                , sep = "_"), ".csv")
         , sep = ";", dec = ",", na = NA)
}
