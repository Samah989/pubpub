#' Gets percentage from MCSO question
#' 
#' Gets percentage of values in a MCSO question with specific value
#' 
#' @param item Data vector from MCSO question
#' @param value The particular value being matched
#' @param digits The number of digits after the decimal for rounding the
#' percent
#' 
#' @examples 
#' # Gets the percent of people who answered "Male" as an option
#' get_mcmopct(survey$Q4, "Male")
#' # Rounds to the nearest integer
#' get_mcmopct(survey$Q4, "Male", digits=0)
#' 
#' @export
get_mcsopct <- function(item, value, digits=2) {
  return(as.numeric(round(digits=digits, 100*prop.table(table(item))[value])))
}