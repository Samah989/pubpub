#' Escape
#' 
#' Escapes special characters
escape_chars <- function(string) {
  specials <- c("\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "{")
  for(char in specials) {
    string <- gsub(char, paste0("\\", char), string, fixed=T)
  }
  return(string)
}