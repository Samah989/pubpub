#' Gets options for MCMO questions
#' 
#' Parses data from an MCMO question and returns a vector of possible options
#' 
#' @param item Data vector from MCMO question
#' @param delim Delimiter for multiple selections, default is a comma
#' @param omit Vector of options which should be ignored
#' @param escape Escapes R special characters, default is TRUE
#' 
#' # Get options from MCMO question "Q3" in "survey" data.frame object
#' get_mcmoopts(survey$Q3)
#' # Omit "Computer science" option
#' get_mcmoopts(survey$Q3, omit="Computer science")
#' 
#' @export
get_mcmoopts <- function(item, delim=",", omit=c(), escape=T) {
  l <- levels(factor(item))
  l <- unique(unlist(strsplit(l, delim)))
  omit <- c("", omit)
  l <- setdiff(l, omit)
  if(escape) {
    l <- escape_chars(l)
  }
  return(l)
}