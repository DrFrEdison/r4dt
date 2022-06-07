spectra.validation.pls.spc.LG3 <- function(pca.input, pls.pred.input, limitvalue = 2, ncomp = pca.input$ncomp){
  
  val.spc <- rep("empty", nrow(pls.pred.input$scores))
  
  valid <- as.numeric(which(pls.pred.input$T2[ , ncomp] < pca.input$T2lim[ limitvalue , ncomp] &
                              pls.pred.input$Q[ , ncomp] < pca.input$Qlim[ limitvalue , ncomp]))
  
  critical <- as.numeric(which(pls.pred.input$T2[ , ncomp] >= pca.input$T2lim[ limitvalue , ncomp] |
                                 pls.pred.input$Q[ , ncomp] >= pca.input$Qlim[ limitvalue , ncomp]))
  
  invalid <- as.numeric(which(pls.pred.input$T2[ , ncomp] >= pca.input$T2lim[ limitvalue , ncomp] &
                                pls.pred.input$Q[ , ncomp] >= pca.input$Qlim[ limitvalue , ncomp]))
  
  val.spc[valid] <- "valid"
  val.spc[critical] <- "critical"
  val.spc[invalid] <- "invalid"
  
  return(val.spc)
}

spectra.validation.pca.spc <- function(pca.input, limitvalue = 2, ncomp = 1){
  
  val.spc <- rep("empty", nrow(pca.input$calres$scores))
  
  valid <- as.numeric(which(pca.input$calres$T2[ , ncomp] < pca.input$T2lim[ limitvalue , ncomp] &
                              pca.input$calres$Q[ , ncomp] < pca.input$Qlim[ limitvalue , ncomp]))
  
  critical <- as.numeric(which(pca.input$calres$T2[ , ncomp] >= pca.input$T2lim[ limitvalue , ncomp] |
                                 pca.input$calres$Q[ , ncomp] >= pca.input$Qlim[ limitvalue , ncomp]))
  
  invalid <- as.numeric(which(pca.input$calres$T2[ , ncomp] >= pca.input$T2lim[ limitvalue , ncomp] &
                                pca.input$calres$Q[ , ncomp] >= pca.input$Qlim[ limitvalue , ncomp]))
  
  val.spc[valid] <- "valid"
  val.spc[critical] <- "critical"
  val.spc[invalid] <- "invalid"
  
  
  return(val.spc)
}