#' Gets percentages in a numeric variable
#' 
#' This function gets the percent of values in a range, inclusive
#' @export
get_numericpct <- function(item, min, max=min, digits=2) {
  item <- item[!is.na(item)]
  return(round(100*(sum(item>=min&item<=max)/length(item)), digits=digits))
}