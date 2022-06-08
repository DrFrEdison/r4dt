pca.plot <- function(pca.input, limitvalue = 2, ncomp = 1){
  xlimp <- c(0, max(xmax <- max(pca.input$calres$T2[ , ncomp], na.rm = T), xT2 <-  pca.input$T2lim[ limitvalue, ncomp]))
  ylimp <- c(0, max(ymax <- max(pca.input$calres$Q[ , ncomp], na.rm = T), yQ <- pca.input$Qlim[ limitvalue, ncomp]))
  
  colp <- rep("pink", length(pca.input$calres$scores[,1]))
  val <- rep(NA, length(pca.input$calres$scores[,1]))
  
  green <- as.numeric(which(pca.input$calres$T2[ , ncomp] < pca.input$T2lim[ limitvalue , ncomp] &
                              pca.input$calres$Q[ , ncomp] < pca.input$Qlim[ limitvalue , ncomp]))
  
  yellow <- as.numeric(which(pca.input$calres$T2[ , ncomp] >= pca.input$T2lim[ limitvalue , ncomp] |
                               pca.input$calres$Q[ , ncomp] >= pca.input$Qlim[ limitvalue , ncomp]))
  
  red <- as.numeric(which(pca.input$calres$T2[ , ncomp] >= pca.input$T2lim[ limitvalue , ncomp] &
                            pca.input$calres$Q[ , ncomp] >= pca.input$Qlim[ limitvalue , ncomp]))

  colp[green] <- "darkgreen"
  colp[yellow] <- "orange"
  colp[red] <- "red"
  
  val[green] <- "valid"
  val[yellow] <- "critical"
  val[red] <- "invalid"
  
  val <- factor(val, levels = c("valid", "critical", "invalid"))
  
  returnlist <- list(xlimp, ylimp, colp, val)
  names(returnlist) <- c("xlimp", "ylimp", "colp", "val")
  return(returnlist)
}

pls.plot <- function(pls.model.input, pls.pred.input, limitvalue = 2, ncomp = pls.model.input$ncomp){
  xlimp <- c(0, max(xmax <- max(pls.pred.input$T2[ , ncomp], na.rm = T), xT2 <-  pls.model.input$T2lim[ limitvalue, ncomp]))
  ylimp <- c(0, max(ymax <- max(pls.pred.input$Q[ , ncomp], na.rm = T), yQ <- pls.model.input$Qlim[ limitvalue, ncomp]))
  
  colp <- rep("pink", nrow(pls.pred.input$scores))
  
  green <- as.numeric(which(pls.pred.input$T2[ , ncomp] < pls.model.input$T2lim[ limitvalue , ncomp] &
                              pls.pred.input$Q[ , ncomp] < pls.model.input$Qlim[ limitvalue , ncomp]))
  
  yellow <- as.numeric(which(pls.pred.input$T2[ , ncomp] >= pls.model.input$T2lim[ limitvalue , ncomp] |
                               pls.pred.input$Q[ , ncomp] >= pls.model.input$Qlim[ limitvalue , ncomp]))
  
  red <- as.numeric(which(pls.pred.input$T2[ , ncomp] >= pls.model.input$T2lim[ limitvalue , ncomp] &
                            pls.pred.input$Q[ , ncomp] >= pls.model.input$Qlim[ limitvalue , ncomp]))
  
  colp[green] <- "darkgreen"
  colp[yellow] <- "orange"
  colp[red] <- "red"
  
  returnlist <- list(xlimp, ylimp, colp)
  names(returnlist) <- c("xlimp", "ylimp", "colp")
  return(returnlist)
}
