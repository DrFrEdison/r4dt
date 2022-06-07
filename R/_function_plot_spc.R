# transfer_csv <- ccep_ccc_dk_coffein$model$hand_DorstenH2O_destH2O
# colp_dat = "Probe"
# export_html = T
# plot_name = "ccep_ccc_dk_coffein_hand_DorstenH2o_destH2O"
# derivative = "spc"
# range_x = NA
# range_y = NA
# immediately = F
# export_html = T


plot_plotly_spc <- function(transfer_csv, colp_dat = NA, derivative = "spc", range_x = NA, range_y = NA, plot_name = NA,
                            immediately = T, export_html =F, legend_name = 1, col_type = "diff", colp = .colp){
  
  if(is.na(plot_name)) plot_name <- "spc_plot"
  
  data_to_plot <- transfer_csv$data
  
  
  
  if(!is.na(colp_dat)){
    factor_v <- as.factor(as.character(transfer_csv$data[,which(names(transfer_csv$data) == colp_dat)]))
    
    if(col_type == "gradient") factor_v <- factor(as.character(transfer_csv$data[,which(names(transfer_csv$data) == colp_dat)]), levels=unique(as.character(transfer_csv$data[,which(names(transfer_csv$data) == colp_dat)])), ordered=TRUE)
    factor_l <- levels(factor_v)
    
    if(col_type == "diff") colp <- colp[1:length(factor_l)]
    if(col_type == "gradient"){
      colfun <- colorRampPalette(c("red", "green", "blue"))
      colp <- colfun(length(factor_l))
    }
    colp2 <- colp[factor_v]}
  
  if(is.na(colp_dat)) colp <- "blue"
  if(is.na(colp_dat)) colp2 <- rep("blue",nrow(data_to_plot))
  
  if(derivative == "spc")  spc_to_plot <- transfer_csv$spc
  if(derivative == "1st")  spc_to_plot <- transfer_csv$spc1st
  if(derivative == "2nd")  spc_to_plot <- transfer_csv$spc2nd
  
  if(any(!is.na(range_y))) range = c(range_y[1], range_y[2])
  if(any(is.na(range_y))) range = c(round(range(unlist(spc_to_plot))[1]*0.975,-1),round(range(unlist(spc_to_plot))[2]*1.025,1))
  
  if(any(!is.na(range_x))) range_xx = c(range_x[1], range_x[2])
  if(any(is.na(range_x))) range_xx = c(range(transfer_csv$wl)[1], range(transfer_csv$wl)[2])
  
  plotlydat <- list()
  plotlydat$au <- list(yp = list(title="AU",showline=T,showgrid=F,mirror=T
                                 ,range = range
                                 ,ticks="outside"),
                       xp = list(title = "lambda/nm",showline=T,showgrid=F,mirror=T,ticks="outside", range = range_xx))
  plotly_au<-plot_ly(type="scatter", mode="lines")%>%layout(yaxis=plotlydat$au$yp, xaxis=plotlydat$au$xp,font=list(size=plotlydat$sizep))
  
  for(i in 1:nrow(data_to_plot)){ plotly_au <- plotly_au %>% add_trace(x = transfer_csv$wl, y= as.numeric(spc_to_plot[i,]),line=list(color=colp2[i]),
                                                                       name= ifelse(nchar(as.character(data_to_plot[i,legend_name]))>30, substr(as.character(data_to_plot[i,legend_name]),1,30), as.character(data_to_plot[i,legend_name])),
                                                                       text=paste(names(data_to_plot)[1],"=",data_to_plot[i,1],
                                                                                  if(length(names(data_to_plot))>1)"<br>",names(data_to_plot)[2],"=",data_to_plot[i,2],
                                                                                  if(length(names(data_to_plot))>2)"<br>",names(data_to_plot)[3],"=",data_to_plot[i,3],
                                                                                  if(length(names(data_to_plot))>3)"<br>",names(data_to_plot)[4],"=",data_to_plot[i,4],
                                                                                  if(length(names(data_to_plot))>4)"<br>",names(data_to_plot)[5],"=",data_to_plot[i,5],
                                                                                  if(length(names(data_to_plot))>5)"<br>",names(data_to_plot)[6],"=",data_to_plot[i,6],
                                                                                  if(length(names(data_to_plot))>6)"<br>",names(data_to_plot)[7],"=",data_to_plot[i,7],
                                                                                  if(length(names(data_to_plot))>7)"<br>",names(data_to_plot)[8],"=",data_to_plot[i,8],
                                                                                  if(length(names(data_to_plot))>8)"<br>",names(data_to_plot)[9],"=",data_to_plot[i,9],
                                                                                  if(length(names(data_to_plot))>9)"<br>",names(data_to_plot)[10],"=",data_to_plot[i,10]))
                                                                       }
  if(export_html==T) htmlwidgets::saveWidget(as_widget(plotly_au),paste0(plot_name,".html"))
  if(immediately==T) return(plotly_au)
}

