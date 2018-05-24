#' Returns list of variables matching matters
#' 
#' Searches through all variable names and returns all that match a search
#' term, using regular expressions.
#' 
#' @param d The full data frame
#' @param pattern The pattern to match in the variable name
#' 
#' @examples
#' # Get all variable names including "Q6_"
#' get_matchvars(survey, "Q6_")
#' # Create data frame with only Q6 variables
#' new_data <- survey[get_matchvars(survey, "Q6_")]
#' 
#' @export
get_matchvars <- function(d, pattern) {
  
  # Return list of matching variables
  return(names(d)[grepl(pattern, names(d))])
}