read_unsb_prediction <- function(unsb_raw, show_else = F, write = F){

  unsb <- list()
  unsb$raw <- unsb_raw

  unsb$trs <- lapply( unsb_raw, function( x ) strsplit( x, "\t"))

  unsb$col <- unsb$trs[[ 1 ]]

  unsb$trs[[1]] <- NULL

  unsb$ID <- lapply(unsb$trs, function( x ) unlist(x)[1])
  unsb$ypred <- lapply(unsb$trs, function( x ) unlist(x)[2])
  unsb$ydev <- lapply(unsb$trs, function( x ) unlist(x)[3])

  unsb$ypred <- as.numeric( gsub( ",", ".", unsb$ypred))
  unsb$ydev <- as.numeric( gsub( ",", ".", unsb$ydev))

  unsb$substance <-   substr(unlist(unsb$col[ 1 ])[1]
                             , unlist( gregexpr( "\\(", unlist(unsb$col[ 1 ])[1])) + 1
                             , unlist( gregexpr( "\\,", unlist(unsb$col[ 1 ])[1])) - 1)

  unsb$ncomp <-   substr(unlist(unsb$col[ 1 ])[1]
                             , unlist( gregexpr( "\\,", unlist(unsb$col[ 1 ])[1])) + 2
                             , unlist( gregexpr( "\\)", unlist(unsb$col[ 1 ])[1])) - 1)
  unsb$ncomp <- gsub( "-", "", substr(unsb$ncom, nchar(unsb$ncom) - 1, nchar(unsb$ncom)))

  if(!show_else) unsb$return <- data.frame(Software = "Unscrambler", ncomp = unsb$ncomp, Y_Pred = unsb$ypred )
  if(show_else) unsb$return <- data.frame(Software = "Unscrambler", ncomp = unsb$ncomp, Y_Pred = unsb$ypred, Y_Dev = unsb$ydev)

  if( write ) write.csv2(unsb$return, paste0(date(), "_UNSB_Prediction.csv"), row.names = F)
  return(  unsb$return )
}
