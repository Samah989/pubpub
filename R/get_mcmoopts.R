#' Gets options for multiple choice, multiple option question
#' 
#' This function gets all options for a multiple choice, multiple option question
#' @export
get_mcmoopts <- function(item, delim=",", omit=c()) {
  l <- levels(factor(item))
  l <- unique(unlist(strsplit(l, delim)))
  omit <- c("", omit)
  l <- setdiff(l, omit)
  return(l)
}