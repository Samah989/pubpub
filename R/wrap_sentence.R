#' A string wrapping function
#' 
#' This function takes a space-delimited sentence and inserts newlines to wrap the sentence. 
#' 
#' @param string The single string
#' @param width The number of characters to limit the lines of the string
#' 
#' @examples 
#' # Wraps a long string
#' wrap_sentence("My bonnie lies over the ocean", width=8)
#' 
#' @export
wrap_sentence <- function(string, width=30) {
  
  # Splits the sentence by spaces
  words <- unlist(strsplit(string, " "))
  
  # Initializes both the final sentence and a string for checking length
  fullsentence <- ""
  checklen <- ""
  
  # Loops through each word
  for(i in 1:length(words)) {
    
    # Add the word to the checking string
    checklen <- paste(checklen, words[i])
    
    # If the checking string is too long
    if(nchar(checklen)>width) {
      
      # Add a new line to the full sentence and reset checking string with word
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- words[i]
    }
    
    # Add the word to the full sentence
    fullsentence <- paste(fullsentence, words[i])
  }
  
  # Removes extra spaces
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  
  # Returns full sentence
  return(fullsentence)
}