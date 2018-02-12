#' Gets list of variables with a certain substring in them
#' 
#' Searches through all variable names and returns all that match a search term
#' @export
get_matchvars <- function(d, pattern) {
  return(names(d)[grepl(pattern, names(d))])
}