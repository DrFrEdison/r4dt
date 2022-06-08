bias <- function(LG3_value, bias_old, reference_value, LG = 3){
  if(LG == 3) bias_new <- LG3_value + bias_old - reference_value
  if(LG == 2) bias_new <- - LG3_value + bias_old + reference_value
  return(bias_new)
}
