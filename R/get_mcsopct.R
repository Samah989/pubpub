#' Gets percentage from MCSO question
#' 
#' Gets percentage of values in a multiple choice question with specific value
#' @export
get_mcsopct <- function(item, value, digits=2) {
  return(as.numeric(round(digits=digits, prop.table(table(item))[value])))
}