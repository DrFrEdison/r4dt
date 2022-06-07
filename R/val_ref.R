spectra.validation.ref.LG3 <- function(spc2nd
                                   , wl = 190:598
                                   , wlchoose = c(230:260)
                                   , max.low.invalid = 150, max.up.invalid = 450
                                   , max.low.critical = 300, max.up.critical = 420
                                   , min.invalid = -50
                                   , min.critical = -300){
  spc2nd.val.ref <- list()
  
  wlrange <- which(wl %in% wlchoose)
  
  spc2nd <- as.numeric(spc2nd)
  
  spc2nd.val.ref$min <- min(spc2nd[ wlrange ], na.rm = T)
  spc2nd.val.ref$max <- max(spc2nd[ wlrange ], na.rm = T)
  
  if(!is.na(spc2nd.val.ref$min)){
    if(spc2nd.val.ref$max >= max.low.critical & spc2nd.val.ref$max <= max.up.critical & spc2nd.val.ref$min <= min.critical) spc2nd.val.ref$val = "valid"
    if(spc2nd.val.ref$max < max.low.critical | spc2nd.val.ref$max > max.up.critical) spc2nd.val.ref$val = "critical"
    if(spc2nd.val.ref$min > min.critical) spc2nd.val.ref$val = "critical"
    
    if(spc2nd.val.ref$min > min.invalid) spc2nd.val.ref$val = "invalid"
    
    if(spc2nd.val.ref$max < max.low.critical | spc2nd.val.ref$max > max.up.critical) spc2nd.val.ref$val = "invalid"
  }
  if(is.na(spc2nd.val.ref$min)) spc2nd.val.ref$val = "empty"
  
  spc2nd.val.ref$val <- factor(spc2nd.val.ref$val, levels = c("valid", "critical", "invalid", "empty"))
  
  return(spc2nd.val.ref$val)
}