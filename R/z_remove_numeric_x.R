remove_numeric_x <- function(dat){
  suppressWarnings(
    names(dat)[which(!is.na(as.numeric(gsub("X", "", names(dat)))))] <-
      gsub("X","",names(dat)[which(!is.na(as.numeric(gsub("X", "", names(dat)))))])
  )
  return(dat)
}
