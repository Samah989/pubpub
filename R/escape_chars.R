#' Escape special characters
#' 
#' Escapes special characters using R-specific escaping syntax ("\\").
escape_chars <- function(string) {
  
  # List of special characters
  specials <- c("\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "{")
  
  # Replaces all characters
  for(char in specials) {
    string <- gsub(char, paste0("\\", char), string, fixed=T)
  }
  
  # Return string
  return(string)
}