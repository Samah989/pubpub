#' Gets percentage from MCMO question
#' 
#' Gets percentage of values in a multiple choice question with specific value
#' @export
get_mcmopct <- function(item, value, digits=2) {
  marker <- ifelse(grepl(value, item), 1, 0)
  return(round(digits=digits, 100*mean(marker, na.rm=T)))
}