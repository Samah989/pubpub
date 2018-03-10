#' Gets percentages in a numeric variable
#' 
#' This function gets the percent of values in a range, inclusive. Defaults to
#' the total percent matching a specific value if no maximum value is given.
#' 
#' @param item Data vector from numeric question
#' @param min The minimum value (inclusive) from the search range
#' @param max The maximum value (inclusive) from the search range, defaults to
#' the minimum if not included
#' @param digits The number of digits after the decimal for rounding the
#' percent
#' 
#' @examples
#' # Gets the percent of people with exactly three dogs
#' get_numericpct(survey$numdogs, min=3)
#' # Gets the percent of people aged 18 to 25, inclusive
#' get_numericpct(survey$age, min=18, max=25)
#' 
#' @export
get_numericpct <- function(item, min, max=min, digits=2) {
  item <- item[!is.na(item)]
  return(round(100*(sum(item>=min&item<=max)/length(item)), digits=digits))
}