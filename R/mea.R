mea <- function(au, ref, drk){
  return( ( as.numeric( as.numeric(ref) - as.numeric(drk)) * 10 ^  as.numeric(-au) ) )
}