pls_analyse_plot <- function(pls_function_obj,
                             model_matrix = NA,
                             colp = NA,
                             wl1, wl2, wl3, wl4,
                             ncomp,
                             derivative,
                             pc_scores = c(1,2),
                             plot_loadings = T,
                             pl_regression_and_pred_vs_ref = ncomp,
                             var_xy = "y",
                             val = F,
                             pngname = paste0(.date(), "_PC", ncomp, "_", derivative, "_", wl1, "_", wl2, "_", wl3, "_", wl4
                                              , "_PC", pc_scores[1], "_vs_PC", pc_scores[2])){
  
  if(ncomp == 1) pc_scores = c(1,1)
  
  if(length(pls_function_obj) < 5){
    plstochoose <- pls_function_obj[[grep(derivative, names(pls_function_obj))[1]]]
    if(is.na(wl3)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, sep="_"),names(plstochoose))]]
    if(!is.na(wl3)) plstochoose <- plstochoose[[grep(paste(wl1, wl2, wl3, wl4, sep="_"),names(plstochoose))]]
  } 
  
  if(length(pls_function_obj) > 5){ plstochoose <- pls_function_obj}
  # Explained Variance Y
  explvar_y <- c(round(drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)*100,1)[1],
                 round(diff(drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)*100),1))
  png(filename = paste0(pngname,".png"),width = 7*1.5, height = 7/1.1,type="cairo",units="in",pointsize=12,res=500)
  par(cex.axis = 1.2, cex.lab=1.5, font.lab=2)
  
  layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
  if(any(!is.na(model_matrix))){
    layout(matrix(c(1,2,3,
                    4,5,3), ncol = 3, byrow =T), heights = c(3,3), widths = c(3, 3, 1))
  } else{
    layout(matrix(c(1,2,
                    3,4), ncol = 2, byrow =T)
    )
  }
  par(mar = c(4,5,1,1))
  
  if(any(!is.na(model_matrix))){
    colp1 <- rainbow(length(levels(factor(model_matrix$data[,grep(colp, names(model_matrix$data))[1]]))))
    colp2 <- colp1[factor(model_matrix$data[,grep(colp, names(model_matrix$data))[1]])]
    legendt <- paste(levels(factor(model_matrix$data[,grep(colp, names(model_matrix$data))[1]])))} else{
      colp1 <- "blue"
      colp2 <- "blue"
    }
  
  plot(plstochoose$scores[,pc_scores[1]], plstochoose$scores[,pc_scores[2]],
       xlab = paste0("PC",pc_scores[1], " (",round(explvar(plstochoose)[pc_scores[1]],1),"%, ", explvar_y[pc_scores[1]],"%)"), 
       ylab = paste("PC",pc_scores[2], "(",round(explvar(plstochoose)[pc_scores[2]],1),"%, ", explvar_y[pc_scores[2]],"%)"),
       cex = 1.75, pch = 20, col = colp2, main = "Score Plot")
  
  if(plot_loadings == F){
  if(is.na(wl3))
  plot(wl1:wl2,plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
       xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))
  if(!is.na(wl3))
    plot(c(wl1:wl2, wl3:wl4),plstochoose$coefficients[,,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
         xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref))
  }
  
  if(plot_loadings == T){
    if(is.na(wl3))
      plot(wl1:wl2,plstochoose$loadings[,pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Regression coefficient, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
    if(!is.na(wl3))
      plot(c(wl1:wl2, wl3:wl4),plstochoose$loadings[ , pl_regression_and_pred_vs_ref[1]], lwd = 2, col = "blue", type = "l", lty = 1,
           xlab = "Lambda in nm", ylab = paste("Loadings, PC",pl_regression_and_pred_vs_ref), main = paste("Loadings, PC",pl_regression_and_pred_vs_ref[1]))
  }
  if(any(!is.na(model_matrix))){
    par(mar = c(0,0,0,0))
    plot(1,1,type="n", axes = F, xlab = "", ylab = "")
    legend("left", legendt, col = colp1, pch = 20, cex = 1.5, bty = "n", ncol = ceiling(length(legendt)/30), xpd = T
           , text.width	= .1, pt.cex = 1.5)
  }
  
  par(mar = c(4,5,1,1))
  if(var_xy == "x") plot(0:ncomp,as.numeric(c(0,cumsum(explvar(plstochoose)[1:ncomp]))), type = "b", col ="blue", lwd = 2,
                      xlab = "Factors", ylab = "X-Variance", main = "Explained Variance")
  if(var_xy == "y") plot(0:ncomp, as.numeric(c(0,drop(R2(plstochoose, estimate = "train", intercept = FALSE)$val)[1:ncomp])*100), type = "b", col ="blue", lwd = 2,
                      xlab = "Factors", ylab = "Y-Variance", main = "Explained Variance")
  
  plot(plstochoose$model$`pls$x`,
       plstochoose$fitted.values[,,pl_regression_and_pred_vs_ref],
       col = colp2, pch = 20, cex = 1.25,
       xlab = paste0("Reference Y, PC ",pl_regression_and_pred_vs_ref),
       ylab = paste0("Predicted Y, PC ",pl_regression_and_pred_vs_ref),
       main = "Predicted vs. Reference")
  
  if(val == T){
    points(plstochoose$model$`pls$x`,
           plstochoose$validation$pred[,,pl_regression_and_pred_vs_ref],
           col = "red", pch = 19, cex = 1.75,
           )
  }
  dev.off()
}
