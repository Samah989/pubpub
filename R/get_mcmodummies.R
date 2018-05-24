#' Creates MCMO dummy variables
#' 
#' This function creates dummy variables for a multiple choice, multiple option
#' question. Suggested data check: Run \code{get_mcmoopts()} first to see what
#' survey choices are being found, if you are not using custom survey choices.
#' 
#' @param item Data vector from MCMO question
#' @param ... Other options as in \code{get_mcmoopts()}
#' 
#' @examples 
#' # Print dummy variables of MCMO question "Q3" in "survey" data.frame object
#' get_mcmodummies(survey$Q3)
#' # Omit "Computer science" option
#' get_mcmodummies(survey$Q3, omit="Computer science")
#' 
#' @export
get_mcmodummies <- function(item, ...) {
  
  # Gets list of options
  l <- get_mcmoopts(item, ...)
  
  # Initializes dummy matrix
  dummies <- c(rep(NA, length(item)))
  
  # Loops through each of the options
  for(pattern in l) {
    
    # Column appends filter to dummy matrix
    marker <- ifelse(grepl(pattern, item), 1, 0)
    marker[item==""] <- NA
    dummies <- cbind(dummies, marker)
  }
  
  # Cleans up dummy matrix
  dummies <- data.frame(dummies[,2:ncol(dummies)])
  names(dummies) <- l
  
  # Return data frame
  return(dummies)
}