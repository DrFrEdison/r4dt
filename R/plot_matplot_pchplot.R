matplots <- function(csv_transfered, derivative = "spc"
                     , col_type = "day", xlimp = NA
                     , seqp = 20, filename=NA, legendpos = NA){

  if(is.na(filename)) filename <- paste0(derivative,"_col_", col_type,"_seq_",seqp)

  wl <- csv_transfered$wl
  if(is.na(xlimp[1])) xlimp <- range(wl)
  wl <- wl[grep(xlimp[1], wl)] : wl[grep(xlimp[2], wl)]

  if(derivative == "spc") spc <- csv_transfered$spc
  if(derivative == "1st") spc <- csv_transfered$spc1st
  if(derivative == "2nd") spc <- csv_transfered$spc2nd

  spc <- t(spc)[
    grep(xlimp[1], rownames(t(spc)[, seq(1, ncol(t(spc)), seqp)])) :
      grep(xlimp[2], rownames(t(spc)[, seq(1, ncol(t(spc)), seqp)])),
    seq(1, ncol(t(spc)), seqp)]

  if(col_type == "day"){
  colp <- rainbow(length(unique(as.Date(csv_transfered$data$date))))
  colp2 <- colp[factor(as.Date(csv_transfered$data$date))]
  colp2 <- colp2[seq(1, length(colp2), seqp)]
  legendt <- as.character(levels(factor(as.Date(csv_transfered$data$date))))
  }

  if(col_type == "week"){

    colp <- rainbow(length(unique(paste(year(as.Date(csv_transfered$data$date)), week(as.Date(csv_transfered$data$date))))))
    colp2 <- colp[factor(week(as.Date(csv_transfered$data$date)))]
    colp2 <- colp2[seq(1, length(colp2), seqp)]
    legendt <- as.character(levels(factor(paste(year(as.Date(csv_transfered$data$date)), week(as.Date(csv_transfered$data$date)), sep = " KW "))))
  }

  if(col_type != "day" & col_type != "week"){
    colp <- rainbow(length(levels(factor(csv_transfered$data[ , grep(col_type, names(csv_transfered$data))[1]]))))
    colp2 <- colp[factor(csv_transfered$data[ , grep(col_type, names(csv_transfered$data))[1]])]
    colp2 <- colp2[seq(1, length(colp2), seqp)]
    legendt <- as.character(levels(factor(csv_transfered$data[ , grep(col_type, names(csv_transfered$data))[1]])))
  }

  if(derivative == "spc") ylabp <- "AU"
  if(derivative == "1st") ylabp <- expression(paste(Delta, " AU / ", Delta, lambda))
  if(derivative == "2nd") ylabp <- expression(paste(Delta, " AU / ", Delta, lambda^2))

  png(paste0(filename, ".png"),width = 7*1.5, height = 7/1.1,type="cairo",units="in",pointsize=12,res=500)
  matplot(wl, spc
          , xlab = "lambda in nm"
          , ylab = ylabp
          , type = "l"
          , lty = 1
          , col =colp2
          , xlim = xlimp
          , ylim = range(spc, na.rm = T))

  if(is.na(legendpos)) legendpos <- "topright"
  legend(legendpos, legendt, lty = 1, col = colp, lwd = 2, ncol = ifelse(length(legendt>4), 4, 2))
  dev.off()
}

pchplots <- function(csv_transfered, derivative = "spc"
                     , col_type = "day"
                     , wl = 273
                     , filename=NA
                     , legendpos = NA){

  if(is.na(filename)) filename <- paste0(derivative, "_lambda_", wl,"_col_", col_type)

  if(derivative == "spc") spc <- csv_transfered$spc
  if(derivative == "1st") spc <- csv_transfered$spc1st
  if(derivative == "2nd") spc <- csv_transfered$spc2nd

  if(col_type == "day"){
    colp <- rainbow(length(unique(as.Date(csv_transfered$data$date))))
    colp2 <- colp[factor(as.Date(csv_transfered$data$date))]
    colp2 <- colp2
    legendt <- as.character(levels(factor(as.Date(csv_transfered$data$date))))
  }

  if(col_type == "week"){
    colp <- rainbow(length(unique(paste(year(as.Date(csv_transfered$data$date)), week(as.Date(csv_transfered$data$date))))))
    colp2 <- colp[factor(week(as.Date(csv_transfered$data$date)))]
    colp2 <- colp2
    legendt <- as.character(levels(factor(paste(year(as.Date(csv_transfered$data$date)), week(as.Date(csv_transfered$data$date)), sep = " KW "))))
  }

  if(col_type != "day" & col_type != "week"){
    colp <- rainbow(length(levels(factor(csv_transfered$data[ , grep(col_type, names(csv_transfered$data))[1]]))))
    colp2 <- colp[factor(csv_transfered$data[ , grep(col_type, names(csv_transfered$data))[1]])]
    legendt <- as.character(levels(factor(csv_transfered$data[ , grep(col_type, names(csv_transfered$data))[1]])))
  }

  if(derivative == "spc") ylabp <- "AU"
  if(derivative == "1st") ylabp <- expression(paste(Delta, " AU / ", Delta, lambda))
  if(derivative == "2nd") ylabp <- expression(paste(Delta, " AU / ", Delta, lambda^2))

  png(paste0(filename, ".png"),width = 7*1.5, height = 7/1.1,type="cairo",units="in",pointsize=12,res=500)
  plot(spc[ , grep(wl, names(spc))]
       , col = colp2
       , pch = 20, cex = .5, axes = F
       , xlab = "", ylab = "AU", main = paste("lambda =", wl, "nm"))
  xaxisdate(as.Date(csv_transfered$data$date), type = "n", formatd = col_type)

  if(is.na(legendpos)) legendpos <- "bottomleft"
  legend(legendpos, legendt, ncol = ifelse(length(legendt>4), 4, 2), col = colp, pch = 20)
  dev.off()
}

pchpredplots <- function(prediction
                        , dayvec = NA
                        , col_type = "day"
                        , filename=NA
                        , legendpos = NA){

  if(is.na(filename)) filename <- paste0("prediction","_col_", col_type)

  if(!is.na(dayvec[1])){
    if(col_type == "day"){
      colp <- rainbow(length(unique(dayvec)))
      colp2 <- colp[factor(dayvec)]
      colp2 <- colp2
      legendt <- as.character(levels(factor(dayvec)))
    }

    if(col_type == "week"){
      colp <- rainbow(length(unique(paste(year(dayvec), week(dayvec)))))
      colp2 <- colp[factor(week(dayvec))]
      colp2 <- colp2
      legendt <- as.character(levels(factor(paste(year(dayvec), week(dayvec), sep = " KW "))))
    }
  }

  if(is.na(dayvec[1])) colp <- "blue"
  if(is.na(dayvec[1])) colp2 <- "blue"

  png(paste0(filename, ".png"),width = 7*1.5, height = 7/1.1,type="cairo",units="in",pointsize=12,res=500)
  plot(prediction
       , col = colp2
       , pch = 20, cex = .5, axes = F
       , xlab = "", ylab = "AU", main = paste("Prediction"))
  xaxisdate(dayvec, type = "n", formatd = col_type)
  if(is.na(legendpos)) legendpos <- "bottomleft"
  legend(legendpos, legendt, ncol = ifelse(length(legendt>4), 4, 2), col = colp, pch = 20)
  dev.off()
}

