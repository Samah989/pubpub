#' Creates dummy variables for multiple choice, multiple option question
#' 
#' This function creates dummy variables for a multiple choice, multiple option question
#' @export
create_mcmodummies <- function(item, ...) {
  l <- get_mcmoopts(item, ...)
  dummies <- c(rep(NA, length(item)))
  for(i in 1:length(l)) {
    pattern <- gsub("\\(", "\\\\(", l[i])
    marker <- ifelse(grepl(pattern, item), 1, 0)
    marker[item==""] <- NA
    dummies <- cbind(dummies, marker)
  }
  dummies <- data.frame(dummies[,2:ncol(dummies)])
  names(dummies) <- l
  return(dummies)
}