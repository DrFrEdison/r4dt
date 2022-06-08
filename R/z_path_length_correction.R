path_length_factor <- function(FG_oder_Sirup = "FG", pl_spc, pl_to_convert, FG, Sirup){

  if(FG_oder_Sirup == "FG") fac2 <- FG / Sirup
  if(FG_oder_Sirup != "FG") fac2 <- Sirup / FG

  return(pl_to_convert / pl_spc * fac2)
}
