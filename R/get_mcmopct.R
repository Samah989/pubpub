#' Gets percentage from MCMO question
#' 
#' Gets percentage of values in a MCMO question with specific value. It is
#' more often used for data checking or reporting single numbers in a document.
#' 
#' @param item Data vector from MCMO question
#' @param value The particular value being matched
#' @param digits The number of digits after the decimal for rounding the
#' percent, default is 2
#' 
#' @examples 
#' # Gets the percent of people who answered "Computer science" as an option
#' get_mcmopct(survey$Q3, "Computer science")
#' # Rounds to the nearest integer
#' get_mcmopct(survey$Q3, "Computer science", digits=0)
#' 
#' @export
get_mcmopct <- function(item, value, digits=2) {
  
  # Creates vector for matching values
  marker <- ifelse(grepl(value, item), 1, 0)
  
  # Returns percentage
  return(round(digits=digits, 100*mean(marker, na.rm=T)))
}