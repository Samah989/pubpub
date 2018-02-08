#' Graphs a multiple choice, single option question
#' 
#' This function creates a horizontal bar graph for a multiple choice, single option question
#' @export
graph_mcso <- function(item, sort="alpha", custom=NULL) {
  sorted <- data.frame(table(item))
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
    sorder <- custom
  } else {
    stop("sort parameter must equal \"alpha\", \"ascending\", or \"descending\"")
  }
  sorted$item <- factor(sorted$item, ordered=T, levels=sorder)
  return(ggplot(sorted, aes(x=item, y=Freq)) +
           geom_bar(stat="identity") +
           xlab("") +
           ylab("Count") +
           coord_flip() +
           theme)
}