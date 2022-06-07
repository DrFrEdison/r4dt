model_parameter <- function(customer, beverage, LG){
  
  originalwd <- getwd()
  require(readODS)
  setwd(unlist(wd$model)[1])
  model_overview <-  read_ods("dt_model_overview.ods")
  
  model_overviewp <- model_overview[model_overview$customer==customer & model_overview$beverage==beverage & model_overview$LG==LG,]
  
  setwd(originalwd)
  return(model_overviewp)
}