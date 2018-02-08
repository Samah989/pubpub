#' Returns text responses to a text-based question
#' 
#' This function returns, in a vector, all of the text responses from a text-based question 
get_textresp <- function(item, rm.nl=F, empty=NA) {
  text <- as.character(item[item!=empty])
  if(rm.nl==T) {
    text <- gsub("\n", " ", text)
  }
  return(text)
}