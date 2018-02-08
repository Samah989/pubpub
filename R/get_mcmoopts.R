#' Gets options for multiple choice, multiple option question
#' 
#' This function gets all options for a multiple choice, multiple option question
#' @export
get_mcmoopts <- function(item, delim=",", rm=c()) {
  l <- levels(factor(item))
  l <- unique(unlist(strsplit(l, delim)))
  l <- setdiff(l, rm)
  return(l)
}