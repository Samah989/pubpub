#' Returns text responses to an open response question
#' 
#' This function returns, in a vector, all of the text responses from a 
#' open responses, text-based question.
#' 
#' @param item Data vector from open responses question
#' @param rm.nl Option for stripping responses of newline characters
#' @param empty Specifies what an empty response looks like
#' 
#' @examples
#' # Returns all "other" responses from question 6
#' get_textresp(survey$Q6_other)
#' 
#' @export
get_textresp <- function(item, rm.nl=F, empty=NA) {
  text <- as.character(item[item!=empty])
  if(rm.nl==T) {
    text <- gsub("\n", " ", text)
  }
  return(text)
}