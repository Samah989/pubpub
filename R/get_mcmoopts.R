#' Gets options for MCMO questions
#' 
#' Parses data from an MCMO question and returns a vector of possible options.
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
get_mcmoopts <- function(item, delim=",", omit=c(), custom.respopts=c(), escape=TRUE) {
  
  # If custom options are given
  if(length(custom.respopts) > 0) {
    
    # Escape if option is on
    l <- custom.respopts
    if(escape) {
      l <- escape_chars(l)
    }
    
    # Return list 
    return(l)
  } else {
    
    # Gets all of the unique combinations of options
    l <- levels(factor(item))
    
    # Splits all of the combinations by delimiter
    l <- unique(unlist(strsplit(l, delim)))
    
    # Remove items in omit list
    if(length(omit)>0) {
      l <- setdiff(l, omit) 
    }
    
    # Escape if option is TRUE
    if(escape) {
      l <- escape_chars(l)
    }
    
    # Return list
    return(l)
  }
}