#' Graphs a single numeric question
#' 
#' This function creates a histogram for a numeric question
#' 
#' @param item Data vector from numeric question
#' @param unit String with the unit of the vector
#' @param xmin The lower bound of the x axis
#' @param xmax The upper bound of the x axis
#' @param binwidth The width of histogram bins
#' @param fill Specifies the color of the bars in the graph, see 
#' <http://sape.inf.usi.ch/quick-reference/ggplot2/colour> for color
#' references
#' 
#' @examples 
#' # Graphs a numeric variable
#' graph_numeric(survey$age, "Years")
#' 
#' @export
graph_numeric <- function(item, unit, xmin=0, xmax=max(item)+binwidth, binwidth=1, fill="darkmagenta") {
  data <- data.frame(item)
  names(data) <- "Item"
  xmin <- xmin-1
  return(ggplot(data) +
           geom_histogram(aes(x=Item), binwidth=binwidth, fill=fill) +
           xlab(unit) +
           ylab("Count") +
           scale_x_continuous(expand=c(0, 0), limits=c(xmin, xmax)) +
           scale_y_continuous(expand=c(0, 0)) +
           theme(panel.background=element_blank(),
                 axis.line=element_line(colour="black")))
}