#' A string wrapping function
#' 
#' This function takes a space-delimited sentence and inserts newlines to wrap the sentence. 
#' 
#' @param string The single string
#' @param width The number of characters to limit the lines of the string
#' 
#' @examples 
#' # Wraps a long string
#' wrap_sentence("My bonnie lies over the ocean")
#' 
#' @export
wrap_sentence <- function(string, width=30) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}