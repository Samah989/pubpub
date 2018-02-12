#' Graphs a numeric question
#' 
#' This function creates a histogram for a numeric question
#' @export
graph_numeric <- function(item, unit, xmin=0, xmax=max(item)+binwidth, binwidth=1, fill="darkmagenta") {
  data <- data.frame(item)
  names(data) <- "Item"
  return(ggplot(data) +
           geom_histogram(aes(x=Item), binwidth=1, fill=fill) +
           xlab(unit) +
           ylab("Count") +
           scale_x_continuous(expand=c(0, 0), limits=c(xmin, xmax)) +
           scale_y_continuous(expand=c(0, 0)) +
           theme(panel.background=element_blank(),
                 axis.line=element_line(colour="black")))
}