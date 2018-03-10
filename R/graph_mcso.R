#' Graphs an MCSO question
#' 
#' This function creates a horizontal bar graph for an MCSO option question
#' 
#' @param item Data vector from MCSO question
#' @param sort Specifies the order of the items in the graph. Options include:
#' \code{descending} - in descending order by frequency, \code{ascending} - in
#' ascending order by frequency, \code{alpha} - in alphabetical order,
#' \code{custom} - in some custom order specified in the \code{custom} parameter
#' @param custom If \code{sort="custom"}, the order of the options to be graphed
#' @param fill Specifies the color of the bars in the graph, see 
#' <http://sape.inf.usi.ch/quick-reference/ggplot2/colour> for color
#' references
#' 
#' @examples 
#' # Graphs an MCSO question
#' graph_mcso(survey$Q4, sort="custom", custom=c("Grains", "Fruits", "Vegetables"))
#' 
#' @export
graph_mcso <- function(item, sort="alpha", custom=NULL, fill="dodgerblue4", ...) {
  sorted <- data.frame(table(item))
  sorted$item <- as.character(sorted$item)
  if(sort=="descending") {
    sorted <- sorted[order(sorted$Freq),]
    sorder <- sorted$item
  } else if(sort=="ascending") {
    sorted <- sorted[order(-sorted$Freq),]
    sorder <- sorted$item
  } else if(sort=="alpha") {
    sorted <- sorted[order(sorted$item),]
    sorted <- sorted[rev(1:nrow(sorted)),]
    sorder <- sorted$item
  } else if(sort=="custom") {
    sorder <- custom[rev(1:length(custom))]
  } else {
    stop("sort parameter must equal \"alpha\", \"ascending\", \"descending\", or \"custom\"")
  }
  sorted$item <- wrap_strings(sorted$item)
  sorder <- wrap_strings(sorder)
  sorted$item <- factor(sorted$item, ordered=T, levels=sorder)
  return(ggplot(sorted, aes(x=item, y=Freq)) +
           geom_bar(stat="identity", fill=fill) +
           xlab("") +
           ylab("Count") +
           coord_flip() +
           theme(panel.background=element_blank(),
                 axis.line=element_line(colour="black")))
}