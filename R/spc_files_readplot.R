read_spc_files <- function(directory,baseline=400,pngplot=T,plotlyplot=T,recursive=F,
                          filestext=NA, colp=NA, subfiles = NA){
  spc <- list()
  dw <- list()
  ref <- list()
  au <- list()
  trans <- list()

  for(i in 1:length(directory)){
    setwd(directory[i])
    spc$files[[i]] <- lapply(directory[i],function(x) paste(x,list.files(recursive=recursive,pattern=".spc",include.dirs = T),sep="//"))
    spc$files[[i]] <- lapply(spc$files[[i]],function(x) x[grep("spc",tools::file_ext(x))])
    #orderspc <- order(lapply(spc$files,function(x) file.info(x)$ctime)[[1]])
    spc$fileslow[[i]] <- lapply(directory[i],function(x) enc2native(list.files(recursive=recursive,pattern=".spc",include.dirs = T)))
    spc$fileslow[[i]] <- lapply(spc$fileslow[[i]],function(x) x[grep("spc",tools::file_ext(x))])
  }
  #orderspclow <- order(lapply(spc$fileslow,function(x) file.info(x)$ctime)[[1]])

  spc$files <- unlist(spc$files)
  spc$fileslow <- unlist(spc$fileslow)

  #spc$files <- spc$files[orderspc]
  #spc$fileslow <- spc$files[orderspclow]

  if(length(subfiles)>0 & !is.na(subfiles)) spc$files <- lapply(subfiles, function(x) spc$files[grep(x,spc$fileslow)])
  if(length(subfiles)>0 & !is.na(subfiles)) spc$files <- unlist(spc$files)
  if(length(subfiles)>0 & !is.na(subfiles)) spc$fileslow <- lapply(subfiles, function(x) spc$fileslow[grep(x,spc$fileslow)])
  if(length(subfiles)>0 & !is.na(subfiles)) spc$fileslow <- unlist(spc$fileslow)

  spc$filestext <- spc$fileslow
  spc$filestext <- gsub("TZO4611","",spc$filestext);spc$filestext <- gsub("TZO4612","",spc$filestext);spc$filestext <- gsub(".spc","",spc$filestext)
  spc$filestext <- gsub("__","_",spc$filestext);spc$filestext <- gsub("__","_",spc$filestext);spc$filestext <- gsub("__","_",spc$filestext)

  suppressWarnings( if(!is.na(filestext[1])) spc$filestext <- filestext )

  spc$files <- as.list(spc$files)
  try(
    spc$raw <- lapply(spc$files,function(x) read.spc(x,keys.hdr2data = T, keys.log2data = T, log.txt = TRUE,log.bin = FALSE)) #, keys.hdr2log = T,keys.log2log = T
  )
  if(length(grep("ChannelType",names(spc$raw[[1]]@data)))!=0){
    spc$type <- lapply(spc$raw,function(x) x@data[grep("ChannelType",names(x@data))][1,1])

    if(length(which(spc$type=="RawData"))>0)  spc$type[which(spc$type=="RawData")] <- lapply(spc$raw,function(x) x@data[grep("ChannelName",names(x@data))][1,1])[which(spc$type=="RawData")]

    for(i in which(spc$type=="Normalized")){if(length(grep("Absorbance",names(spc$raw[[i]]@data)))) spc$type[i] <- "Absorbance" else spc$type[i] <- "Transmission"}

    for(i in grep("Dark",spc$type)){
      dw$files[[i]] <- spc$files[i]
      dw$filestext[[i]] <- basename(spc$filestext[i])
      dw$wl[[i]] <- spc$raw[[i]]@wavelength
      dw$data[[i]] <- spc$raw[[i]]@data
      dw$label[[i]] <- spc$raw[[i]]@label
      dw$log[[i]] <- spc$raw[[i]]@log
      dw$spc[[i]] <- t(spc$raw[[i]]@data$spc)
    }

    for(i in grep("Refer",spc$type)){
      ref$files[[i]] <- spc$files[i]
      ref$filestext[[i]] <- basename(spc$filestext[i])
      ref$wl[[i]] <- spc$raw[[i]]@wavelength
      ref$data[[i]] <- spc$raw[[i]]@data
      ref$label[[i]] <- spc$raw[[i]]@label
      ref$log[[i]] <- spc$raw[[i]]@log
      ref$spc[[i]] <- t(spc$raw[[i]]@data$spc)
    }

    for(i in which(spc$type=="Absorbance")){
      au$files[[i]] <- spc$files[i]
      au$filestext[[i]] <- basename(spc$filestext[i])
      au$wl[[i]] <- spc$raw[[i]]@wavelength
      au$data[[i]] <- spc$raw[[i]]@data
      au$label[[i]] <- spc$raw[[i]]@label
      au$log[[i]] <- spc$raw[[i]]@log
      au$spc[[i]] <- t(spc$raw[[i]]@data$spc)
      au$spc_baseline[[i]] <- au$spc[[i]]
    }

    for(i in which(spc$type=="Transmission")){
      trans$files[[i]] <- spc$files[i]
      trans$filestext[[i]] <- basename(spc$filestext[i])
      trans$wl[[i]] <- spc$raw[[i]]@wavelength
      trans$data[[i]] <- spc$raw[[i]]@data
      trans$label[[i]] <- spc$raw[[i]]@label
      trans$log[[i]] <- spc$raw[[i]]@log
      trans$spc[[i]] <- t(spc$raw[[i]]@data$spc)
    }
  } else {

    for(i in grep("-D",spc$filestext)){
      dw$files[[i]] <- spc$files[i]
      dw$filestext[[i]] <- basename(spc$filestext[i])
      dw$wl[[i]] <- spc$raw[[i]]@wavelength
      dw$data[[i]] <- spc$raw[[i]]@data
      dw$label[[i]] <- spc$raw[[i]]@label
      dw$spc[[i]] <- t(spc$raw[[i]]@data$spc)
    }

    for(i in grep("-R",spc$filestext)){
      ref$files[[i]] <- spc$files[i]
      ref$filestext[[i]] <- basename(spc$filestext[i])
      ref$wl[[i]] <- spc$raw[[i]]@wavelength
      ref$data[[i]] <- spc$raw[[i]]@data
      ref$label[[i]] <- spc$raw[[i]]@label
      ref$spc[[i]] <- t(spc$raw[[i]]@data$spc)
    }

    for(i in grep("-M",spc$filestext)){
      au$files[[i]] <- spc$files[i]
      au$filestext[[i]] <- basename(spc$filestext[i])
      au$wl[[i]] <- spc$raw[[i]]@wavelength
      au$data[[i]] <- spc$raw[[i]]@data
      au$label[[i]] <- spc$raw[[i]]@label
      au$spc[[i]] <- t(spc$raw[[i]]@data$spc)
      au$spc_baseline[[i]] <- au$spc[[i]]
    }

    if(is.null(au$spc)){
    for(i in grep("_c",substr(spc$filestext, nchar(spc$filestext) - 5, nchar(spc$filestext)))){
      au$files[[i]] <- spc$files[i]
      au$filestext[[i]] <- basename(spc$filestext[i])
      au$wl[[i]] <- spc$raw[[i]]@wavelength
      au$data[[i]] <- spc$raw[[i]]@data
      au$label[[i]] <- spc$raw[[i]]@label
      au$spc[[i]] <- t(spc$raw[[i]]@data$spc)
      au$spc_baseline[[i]] <- au$spc[[i]]
    }

    }

  }

  for(i in 1:length(au$spc)) if(length(ncol(au$spc[[i]]))>0) au$wl[[i]] <-  replicate(ncol(au$spc[[i]]),au$wl[[i]])
  for(i in 1:length(trans$spc)) if(length(ncol(trans$spc[[i]]))>0) trans$wl[[i]] <-  replicate(ncol(trans$spc[[i]]),trans$wl[[i]])

  nullvecref <- which(lapply(ref$wl,is.null)==F)
  nullvecdw <- which(lapply(dw$wl,is.null)==F)
  nullvectrans <- which(lapply(trans$wl,is.null)==F)
  nullvecau <- which(lapply(au$wl,is.null)==F)

  if(!is.na(baseline)){
    for(i in nullvecau){
      if(ncol(au$spc_baseline[[i]])==1) au$spc_baseline[[i]] <- au$spc_baseline[[i]]-au$spc_baseline[[i]][which.min(abs(au$wl[[i]]-baseline)),]
      if(ncol(au$spc_baseline[[i]])>1){
        for(j in 1:ncol(au$spc_baseline[[i]])){
          au$spc_baseline[[i]][,j] <- au$spc_baseline[[i]][,j]-as.numeric(au$spc_baseline[[i]][,j])[which.min(abs(au$wl[[i]][,j]-baseline))]
        }
      }
    }
  }

  if(is.na(colp)[1]){
    colp2 <- rainbow(length(unique(unlist(spc$files))))
    colp <- colp2[factor(unlist(spc$files))]
  }

  # au plot ####
  if(pngplot==T){
    if(length(nullvecau)>0){
      legendtext_au <- au$filestext[nullvecau]
      if(length(grep("ChannelType",names(spc$raw[[1]]@data)))!=0){
        legendtext_au <- paste(legendtext_au,
                               paste0(" It=",as.numeric(gsub(",",".",unlist(lapply(au$data,function(x) unique(x$It)))))),
                               paste0(" Av=",as.numeric(gsub(",",".",unlist(lapply(au$data,function(x) unique(x$Aver)))))),
                               sep=";")

        if(length(legendtext_au)>20){
          legendtext_au <- gsub("Grundstoff","GS",legendtext_au)
          for(i in 1:20) legendtext_au <- gsub(paste("c01_",i),"",legendtext_au)
          for(i in 1:length(list.dirs(path = ".", full.names = F, recursive = TRUE))) legendtext_au <- gsub(list.dirs(path = ".", full.names = F, recursive = TRUE)[i],"",legendtext_au)
          legendtext_au <- gsub("/","",legendtext_au)
        }
      }
      xlimp <- range(unlist(au$wl))
      ylimp <- range(unlist(au$spc))

      png(filename="absorption.png",width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(5,4,10,1))
      matplot(au$wl[[nullvecau[[1]]]],au$spc[[nullvecau[[1]]]],type="n", ylim=c(ylimp[1],ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="AU")
      for(i in nullvecau) matlines(au$wl[[i]],au$spc[[i]],type="l",col=colp[i])
      legend(min(au$wl[[nullvecau[[1]]]])-60,ylimp[2]+diff(ylimp)*.55,unique(legendtext_au),cex=.6,lty=1,col=unique(colp[nullvecau]),adj=0,xpd=T,
             ncol = length(seq(1,length(legendtext_au),15)),x.intersp = .1)
      dev.off()

      if(!is.na(baseline)){
        png(filename="absorption_baseline.png",width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
        par(mar=c(5,4,10,1))
        matplot(au$wl[[nullvecau[[1]]]],au$spc_baseline[[nullvecau[[1]]]],type="n", ylim=c(ylimp[1],ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
                xlab="Lambda (nm)",ylab="AU")
        for(i in nullvecau) matlines(au$wl[[i]],au$spc_baseline[[i]],type="l",col=colp[i])
        legend(min(au$wl[[nullvecau[[1]]]])-60,ylimp[2]+diff(ylimp)*.55,unique(legendtext_au),cex=.6,lty=1,col=unique(colp[nullvecau]),adj=0,xpd=T,
               ncol = length(seq(1,length(legendtext_au),15)),x.intersp = .1)
        dev.off()}

    }

    # trans plot ####
    if(length(nullvectrans)>0){
      legendtext_trans <- trans$filestext[nullvectrans]
      xlimp <- range(unlist(trans$wl))
      ylimp <- range(unlist(trans$spc))

      if(length(legendtext_trans)>20){
        legendtext_trans <- gsub("Grundstoff","GS",legendtext_trans)
        for(i in 1:20) legendtext_trans <- gsub(paste("c01_",i),"",legendtext_trans)
        legendtext_trans <- gsub(" ","",legendtext_trans)
        legendtext_trans <- gsub("_","",legendtext_trans)
        for(i in 1:length(list.dirs(path = ".", full.names = F, recursive = TRUE))) legendtext_trans <- gsub(list.dirs(path = ".", full.names = F, recursive = TRUE)[i],"",legendtext_trans)
        legendtext_trans <- gsub("/","",legendtext_trans)
      }

      png(filename="transmission.png",width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(5,4,10,1))
      matplot(trans$wl[[nullvectrans[[1]]]],trans$spc[[nullvectrans[[1]]]],type="n", ylim=c(ylimp[1],ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="% Tranmssion")
      for(i in nullvectrans) matlines(trans$wl[[i]],trans$spc[[i]],type="l",col=colp[i])
      legend(min(trans$wl[[nullvectrans[[1]]]])-60,ylimp[2]+diff(ylimp)*.55,unique(legendtext_trans),cex=.6,lty=1,col=unique(colp[nullvectrans]),adj=0,xpd=T,
             ncol = length(seq(1,length(legendtext_trans),15)),x.intersp = .1)
      dev.off()
    }

    # ref plot ####
    if(length(nullvecref)>0){
      legendtext_ref <- ref$filestext[nullvecref]
      legendtext_ref <- paste(legendtext_ref,
                              paste0(" Max=",round(unlist(lapply(ref$spc,max)[nullvecref]),0)),
                              paste0(" 200nm=",round(unlist(mapply(function(x,y) y[min(which(x>199))],ref$wl,ref$spc)[nullvecref]),0)),sep=";")

      if(length(legendtext_ref)>20){
        legendtext_ref <- gsub("Grundstoff","GS",legendtext_ref)
        for(i in 1:20) legendtext_ref <- gsub(paste("c01_",i),"",legendtext_ref)
        legendtext_ref <- gsub(" ","",legendtext_ref)
        legendtext_ref <- gsub("_","",legendtext_ref)
        for(i in 1:length(list.dirs(path = ".", full.names = F, recursive = TRUE))) legendtext_ref <- gsub(list.dirs(path = ".", full.names = F, recursive = TRUE)[i],"",legendtext_ref)
        legendtext_ref <- gsub("/","",legendtext_ref)
      }

      xlimp <- range(unlist(ref$wl))
      ylimp <- range(unlist(ref$spc))
      png(filename="reference.png",width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(5,4,10,1))
      matplot(ref$wl[[nullvecref[[1]]]],ref$spc[[nullvecref[[1]]]],type="n", ylim=c(ylimp[1],ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts")
      for(i in nullvecref) matlines(ref$wl[[i]],ref$spc[[i]],type="l",col=colp[i])
      legend(min(ref$wl[[nullvecref[[1]]]])-60,ylimp[2]+diff(ylimp)*.55,legendtext_ref,cex=.6,lty=1,col=colp[nullvecref],adj=0,xpd=T,
             ncol = length(seq(1,length(legendtext_ref),15)),x.intersp = .1)
      dev.off()
    }

    # dw plot ####
    if(length(nullvecdw)>0){
      legendtext_dw <- dw$filestext[nullvecdw]
      xlimp <- range(unlist(dw$wl))
      ylimp <- range(unlist(dw$spc))

      if(length(legendtext_dw)>20){
        legendtext_dw <- gsub("Grundstoff","GS",legendtext_dw)
        for(i in 1:20) legendtext_dw <- gsub(paste("c01_",i),"",legendtext_dw)
        legendtext_dw <- gsub(" ","",legendtext_dw)
        legendtext_dw <- gsub("_","",legendtext_dw)
        for(i in 1:length(list.dirs(path = ".", full.names = F, recursive = TRUE))) legendtext_dw <- gsub(list.dirs(path = ".", full.names = F, recursive = TRUE)[i],"",legendtext_dw)
        legendtext_dw <- gsub("/","",legendtext_dw)
      }

      png(filename="darkvalue.png",width=7,height=7,type="cairo",units="in",pointsize=12,res=500)
      par(mar=c(5,4,10,1))
      matplot(dw$wl[[nullvecdw[[1]]]],dw$spc[[nullvecdw[[1]]]],type="n", ylim=c(ylimp[1],ylimp[2]),xlim=c(xlimp[1],xlimp[2]),
              xlab="Lambda (nm)",ylab="Counts")
      for(i in nullvecdw) matlines(dw$wl[[i]],dw$spc[[i]],type="l",col=colp[i])
      legend(min(dw$wl[[nullvecdw[[1]]]])-60,ylimp[2]+diff(ylimp)*.55,legendtext_dw,cex=.6,lty=1,col=colp[nullvecdw],adj=0,xpd=T,
             ncol = length(seq(1,length(legendtext_dw),15)),x.intersp = .1)
      dev.off()
    }
  }

  dw$wl <- dw$wl[which(!is.na(dw$files))]
  dw$data <- dw$data[which(!is.na(dw$files))]
  dw$label <- dw$label[which(!is.na(dw$files))]
  dw$log <- dw$log[which(!is.na(dw$files))]
  dw$spc <- dw$spc[which(!is.na(dw$files))]
  dw$files <- dw$files[which(!is.na(dw$files))]
  dw$filestext <- dw$filestext[which(!is.na(dw$filestext))]

  ref$wl <- ref$wl[which(!is.na(ref$files))]
  ref$data <- ref$data[which(!is.na(ref$files))]
  ref$label <- ref$label[which(!is.na(ref$files))]
  ref$log <- ref$log[which(!is.na(ref$files))]
  ref$spc <- ref$spc[which(!is.na(ref$files))]
  ref$files <- ref$files[which(!is.na(ref$files))]
  ref$filestext <- ref$filestext[which(!is.na(ref$filestext))]

  au$wl <- au$wl[which(!is.na(au$files))]
  au$data <- au$data[which(!is.na(au$files))]
  au$label <- au$label[which(!is.na(au$files))]
  au$log <- au$log[which(!is.na(au$files))]
  au$spc <- au$spc[which(!is.na(au$files))]
  au$spc_baseline <- au$spc_baseline[which(!is.na(au$files))] #!#
  au$files <- au$files[which(!is.na(au$files))]
  au$filestext <- au$filestext[which(!is.na(au$filestext))]

  trans$wl <- trans$wl[which(!is.na(trans$files))]
  trans$data <- trans$data[which(!is.na(trans$files))]
  trans$label <- trans$label[which(!is.na(trans$files))]
  trans$log <- trans$log[which(!is.na(trans$files))]
  trans$spc <- trans$spc[which(!is.na(trans$files))]
  trans$files <- trans$files[which(!is.na(trans$files))]
  trans$filestext <- trans$filestext[which(!is.na(trans$filestext))]

  dw$wl <- dw$wl[which(unlist(lapply(dw$wl, function(x) !is.null(x)))==T)]
  dw$data <- dw$data[which(unlist(lapply(dw$data, function(x) !is.null(x)))==T)]
  dw$label <- dw$label[which(unlist(lapply(dw$label, function(x) !is.null(x)))==T)]
  dw$log <- dw$log[which(unlist(lapply(dw$log, function(x) !is.null(x)))==T)]
  dw$spc <- dw$spc[which(unlist(lapply(dw$spc, function(x) !is.null(x)))==T)]
  dw$files <- dw$files[which(unlist(lapply(dw$files, function(x) !is.null(x)))==T)]
  dw$filestext <- dw$filestext[which(unlist(lapply(dw$filestext, function(x) !is.null(x)))==T)]

  ref$wl <- ref$wl[which(unlist(lapply(ref$wl, function(x) !is.null(x)))==T)]
  ref$data <- ref$data[which(unlist(lapply(ref$data, function(x) !is.null(x)))==T)]
  ref$label <- ref$label[which(unlist(lapply(ref$label, function(x) !is.null(x)))==T)]
  ref$log <- ref$log[which(unlist(lapply(ref$log, function(x) !is.null(x)))==T)]
  ref$spc <- ref$spc[which(unlist(lapply(ref$spc, function(x) !is.null(x)))==T)]
  ref$files <- ref$files[which(unlist(lapply(ref$files, function(x) !is.null(x)))==T)]
  ref$filestext <- ref$filestext[which(unlist(lapply(ref$filestext, function(x) !is.null(x)))==T)]

  au$wl <- au$wl[which(unlist(lapply(au$wl, function(x) !is.null(x)))==T)]
  au$data <- au$data[which(unlist(lapply(au$data, function(x) !is.null(x)))==T)]
  au$label <- au$label[which(unlist(lapply(au$label, function(x) !is.null(x)))==T)]
  au$log <- au$log[which(unlist(lapply(au$log, function(x) !is.null(x)))==T)]
  au$spc <- au$spc[which(unlist(lapply(au$spc, function(x) !is.null(x)))==T)]
  au$spc_baseline <- au$spc_baseline[which(unlist(lapply(au$spc_baseline, function(x) !is.null(x)))==T)]
  au$files <- au$files[which(unlist(lapply(au$files, function(x) !is.null(x)))==T)]
  au$filestext <- au$filestext[which(unlist(lapply(au$filestext, function(x) !is.null(x)))==T)]

  trans$wl <- trans$wl[which(unlist(lapply(trans$wl, function(x) !is.null(x)))==T)]
  trans$data <- trans$data[which(unlist(lapply(trans$data, function(x) !is.null(x)))==T)]
  trans$label <- trans$label[which(unlist(lapply(trans$label, function(x) !is.null(x)))==T)]
  trans$log <- trans$log[which(unlist(lapply(trans$log, function(x) !is.null(x)))==T)]
  trans$spc <- trans$spc[which(unlist(lapply(trans$spc, function(x) !is.null(x)))==T)]
  trans$files <- trans$files[which(unlist(lapply(trans$files, function(x) !is.null(x)))==T)]
  trans$filestext <- trans$filestext[which(unlist(lapply(trans$filestext, function(x) !is.null(x)))==T)]

  if(plotlyplot==T){
    plotlyp <- list()
    plotlyp$sizep <- 10
    plotlyp$widthp <- 1

    if(length(nullvecref)>0){
      plotlyp$ref <- list(yp = list(title="Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(ref$spc))[1]*0.975,-1),round(range(unlist(ref$spc))[2]*1.025,-1)),ticks="outside"),
                          xp = list(title = "lambda/nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(ref$wl))[1]*0.975,-1),round(range(unlist(ref$wl))[2]*1.025,-1))))
      plotly_ref<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$ref$yp, xaxis=plotlyp$ref$xp,font=list(size=plotlyp$sizep))
      for(i in 1:length(ref$files)){
        plotly_ref <- plotly_ref%>% plotly::add_trace(
          x=as.numeric(ref$wl[[i]]),y=as.numeric(ref$spc[[i]]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
          name=ref$data[[i]][1,grep("fdate",names(ref$data[[i]]))],
          text=paste("Time",ref$data[[i]][1,grep("fdate",names(ref$data[[i]]))],
                     "<br>File",basename(unlist(ref$files[[i]])),
                     "<br>Iteration",ref$data[[i]][1,grep("It",names(ref$data[[i]]))[1]],
                     "<br>Average",ref$data[[i]][1,grep("Aver",names(ref$data[[i]]))])
        )}
      tryCatch(htmlwidgets::saveWidget(as_widget(plotly_ref), paste0(date(), "_ref.html")),error=function(e){})
    }

    if(all(is.na(filestext)) & length(filestext) == 1) filestext <- rep(NA, length(au$files))

    if(length(nullvecau)>0){
      plotlyp$au <- list(yp = list(title="Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(au$spc))[1]*0.975,-1),round(range(unlist(au$spc))[2]*1.025,-1)),ticks="outside"),
                         xp = list(title = "lambda/nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(au$wl))[1]*0.975,-1),round(range(unlist(au$wl))[2]*1.025,-1))))
      plotly_au<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$au$yp, xaxis=plotlyp$au$xp,font=list(size=plotlyp$sizep))
      for(i in 1:length(au$files)){
        if(length(au$spc[[i]])>0)  for(j in 1:ncol(au$spc[[i]])){
          plotly_au <- plotly_au%>% plotly::add_trace(
            x=as.numeric(au$wl[[i]][,j]),y=as.numeric(au$spc[[i]][,j]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
            name=ifelse(is.na(filestext)[i], as.character(au$data[[i]][1,grep("fdate",names(au$data[[i]]))]), filestext[i]),
            text=paste("Time",au$data[[i]][1,grep("fdate",names(au$data[[i]]))],
                       "<br>File",basename(unlist(au$files[i])),
                       "<br>Iteration",au$data[[i]][1,grep("It",names(au$data[[i]]))[1]],
                       "<br>Average",au$data[[i]][1,grep("Aver",names(au$data[[i]]))])
          )
        }
      }
      tryCatch(htmlwidgets::saveWidget(as_widget(plotly_au),paste0(date(), "_spc.html")),error=function(e){})

      if(!is.na(baseline)){
        plotlyp$au_baseline <- list(yp = list(title="Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(au$spc_baseline))[1]*0.975,-1),round(range(unlist(au$spc_baseline))[2]*1.025,-1)),ticks="outside"),
                                    xp = list(title = "lambda/nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(au$wl))[1]*0.975,-1),round(range(unlist(au$wl))[2]*1.025,-1))))
        plotly_au<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$au_baseline$yp, xaxis=plotlyp$au_baseline$xp,font=list(size=plotlyp$sizep))
        for(i in 1:length(au$files)){
          if(length(au$spc_baseline[[i]])>0)  for(j in 1:ncol(au$spc_baseline[[i]])){
            plotly_au <- plotly_au%>% plotly::add_trace(
              x=as.numeric(au$wl[[i]][,j]),y=as.numeric(au$spc_baseline[[i]][,j]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
              name=ifelse(is.na(filestext)[i], au$data[[i]][1,grep("fdate",names(au$data[[i]]))], filestext[i]),
              text=paste("Time",au$data[[i]][1,grep("fdate",names(au$data[[i]]))],
                         "<br>File",basename(unlist(au$files[i])),
                         "<br>Iteration",au$data[[i]][1,grep("It",names(au$data[[i]]))[1]],
                         "<br>Average",au$data[[i]][1,grep("Aver",names(au$data[[i]]))])
            )
          }
        }
        tryCatch(htmlwidgets::saveWidget(as_widget(plotly_au),paste0(date(), "_spc_bl.html")),error=function(e){})
      }
    }

    if(length(nullvecdw)>0){
      plotlyp$dw <- list(yp = list(title="Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(dw$spc))[1]*0.975,-1),round(range(unlist(dw$spc))[2]*1.025,-1)),ticks="outside"),
                         xp = list(title = "lambda/nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(dw$wl))[1]*0.975,-1),round(range(unlist(dw$wl))[2]*1.025,-1))))
      plotly_dw<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$dw$yp, xaxis=plotlyp$dw$xp,font=list(size=plotlyp$sizep))
      for(i in 1:length(dw$files)){
        plotly_dw <- plotly_dw%>% plotly::add_trace(
          x=as.numeric(dw$wl[[i]]),y=as.numeric(dw$spc[[i]]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
          name=dw$data[[i]][1,grep("fdate",names(dw$data[[i]]))],
          text=paste("Time",dw$data[[i]][1,grep("fdate",names(dw$data[[i]]))],
                     "<br>File",basename(unlist(dw$files[[i]])),
                     "<br>Iteration",dw$data[[i]][1,grep("It",names(dw$data[[i]]))[1]],
                     "<br>Average",dw$data[[i]][1,grep("Aver",names(dw$data[[i]]))])
        )}
      tryCatch(htmlwidgets::saveWidget(as_widget(plotly_dw),paste0(date(), "_drk.html")),error=function(e){})
    }

    if(length(nullvectrans)>0){
      plotlyp$trans <- list(yp = list(title="Counts",showline=T,showgrid=F,mirror=T,range=c(round(range(unlist(trans$spc))[1]*0.975,-1),round(range(unlist(trans$spc))[2]*1.025,-1)),ticks="outside"),
                            xp = list(title = "lambda/nm",showline=T,showgrid=F,mirror=T,ticks="outside",range=c(round(range(unlist(trans$wl))[1]*0.975,-1),round(range(unlist(trans$wl))[2]*1.025,-1))))
      plotly_trans<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlyp$trans$yp, xaxis=plotlyp$trans$xp,font=list(size=plotlyp$sizep))
      for(i in 1:length(trans$files)){
        if(length(trans$spc[[i]])>0)  for(j in 1:ncol(trans$spc[[i]])){
          plotly_trans <- plotly_trans%>% plotly::add_trace(
            x=as.numeric(trans$wl[[i]][,j]),y=as.numeric(trans$spc[[i]][,j]),type="scatter", mode="lines",line=list(width=plotlyp$widthp,color=colp[i]),
            name=trans$data[[i]][1,grep("fdate",names(trans$data[[i]]))],
            text=paste("Time",trans$data[[i]][1,grep("fdate",names(trans$data[[i]]))],
                       "<br>File",basename(unlist(trans$files[i])),
                       "<br>Iteration",trans$data[[i]][1,grep("It",names(trans$data[[i]]))],
                       "<br>Average",trans$data[[i]][1,grep("Aver",names(trans$data[[i]]))])
          )
        }
      }
      tryCatch(htmlwidgets::saveWidget(as_widget(plotly_trans),paste0(date(), "_trans.html")),error=function(e){})
    }
  }
  returnlist <- list(dw,ref,au,trans)
  names(returnlist) <- c("drk","ref","au","trans")
  return(returnlist)
}
