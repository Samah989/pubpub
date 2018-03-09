#' Creates MCMO dummy variables
#' 
#' This function creates dummy variables for a multiple choice, multiple option
#' question. Suggested data check: Run \code{get_mcmoopts()} first to see what
#' survey choices are being found.
#' 
#' @param item Data vector from MCMO question.
#' @param ... Other options as in \code{get_mcmoopts()}
#' 
#' @examples 
#' # Print dummy variables of MCMO question "Q3" in "survey" data.frame object
#' create_mcmodummies(survey$Q3)
#' # Omit "Computer science" option
#' create_mcmodummies(survey$Q3, omit="Computer science")
#' 
#' @export
create_mcmodummies <- function(item, ...) {
  # Gets list of options
  l <- get_mcmoopts(item, ...)
  
  # Creates dummies data frame
  dummies <- c(rep(NA, length(item)))
  for(i in 1:length(l)) {
    pattern <- gsub("\\(", "\\\\(", l[i])
    marker <- ifelse(grepl(pattern, item), 1, 0)
    marker[item==""] <- NA
    dummies <- cbind(dummies, marker)
  }
  dummies <- data.frame(dummies[,2:ncol(dummies)])
  names(dummies) <- l
  
  # Return data frame
  return(dummies)
}