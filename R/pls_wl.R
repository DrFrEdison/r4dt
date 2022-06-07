.wlr_function <- function(wlr1, wlr2, seqp){
  wlr1 <- seq(min(wlr1), max(wlr1),seqp)
  wlr2 <- seq(min(wlr2), max(wlr2),seqp)
  wlr <- expand.grid(wlr1, wlr2)
  if(length(which(wlr[,1] >= wlr[,2])) > 0) wlr <- wlr[-which(wlr[,1] >= wlr[,2]),]
  return(wlr)
}

.wlr_function_multi <- function(wlr1, wlr2, seqp){
  
  wlr3 <- c(min(wlr1), max(wlr2))
  
  wlr1 <- seq(min(wlr1), max(wlr1),seqp)
  wlr2 <- seq(min(wlr2), max(wlr2),seqp)
  wlr3 <- seq(min(wlr3), max(wlr3),seqp)
  wlr4 <- rev(wlr3)
  wlr <- expand.grid(wlr1, wlr2, wlr3, wlr4)
  
  if(length(which(wlr$Var1 >= wlr$Var2)) > 0) wlr <- wlr[-which(wlr$Var1 >= wlr$Var2),]
  if(length(which(wlr$Var3<=wlr$Var2)) >0 ) wlr <- wlr[-which(wlr$Var3<=wlr$Var2) , ]
  if(length(which(wlr$Var3<=wlr$Var1)) >0 ) wlr <- wlr[-which(wlr$Var3<=wlr$Var1) , ]
  if(length(which(wlr$Var4<=wlr$Var3)) >0 ) wlr <- wlr[-which(wlr$Var4<=wlr$Var3) , ]
  
  return(wlr)
}
