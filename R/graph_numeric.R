#' Graphs a single numeric question
#' 
#' This function creates a histogram for a numeric question
#' 
#' @param item Data vector from numeric question
#' @param unit String with the unit of the vector
#' @param xmin The lower bound of the x axis, default is 0
#' @param xmax The upper bound of the x axis, default is the maximum data 
#' value, plus the binwidth
#' @param binwidth The width of histogram bins, default is 1
#' @param fill Specifies the color of the bars in the graph, default is
#' "dark magenta." See <http://sape.inf.usi.ch/quick-reference/ggplot2/colour>
#' for color references
#' 
#' @examples 
#' # Graphs a numeric variable
#' graph_numeric(survey$age, "Years")
#' 
#' @export
graph_numeric <- function(item, unit, xmin=0, xmax=max(item)+binwidth, binwidth=1, fill="darkmagenta") {
  
  # Creates data frame from from item
  data <- data.frame(item)
  names(data) <- "Item"
  
  # Shift minimum to the left
  xmin <- xmin-1
  
  # Return graph
  return(ggplot(data) +
           geom_histogram(aes(x=Item), binwidth=binwidth, fill=fill) +
           xlab(unit) +
           ylab("Count") +
           scale_x_continuous(expand=c(0, 0), limits=c(xmin, xmax)) +
           scale_y_continuous(expand=c(0, 0)) +
           theme(panel.background=element_blank(),
                 axis.line=element_line(colour="black")))
}