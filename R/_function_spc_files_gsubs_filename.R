gsubs <- function(character_vector, sourcetext, basename = F){
  if(basename == T) sourcetext <- basename(as.character(unlist(sourcetext)))
  sourcetext_alternative <- sourcetext
  for(i in 1:length(char_vec))  sourcetext_alternative <- gsub(char_vec[i],"",sourcetext_alternative)
  return(sourcetext_alternative)
}
