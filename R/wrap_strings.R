#' A vector of strings wrapping function
#' 
#' This function applies wrap_sentence to a vector of strings
wrap_strings <- function(vector_of_strings, ...){
  sapply(vector_of_strings,FUN=function(x) {paste(wrap_sentence(x, ...))})
}