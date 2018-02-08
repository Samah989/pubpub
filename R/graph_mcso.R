#' Graphs a multiple choice, single option question
#' 
#' This function creates a horizontal bar graph for a multiple choice, single option question
graph_mcso <- function(item, sort="alpha") {
  sorted <- data.frame(table(item))
  sort="alpha"
  if(sort=="descending") {
    sorted <- sorted[order(sorted$Freq),]
  } else if(sort=="ascending") {
    sorted <- sorted[order(-sorted$Freq),]
  } else if(sort=="alpha") {
    sorted <- sorted[order(sorted$item),]
    sorted <- sorted[rev(1:nrow(sorted)),]
  } else {
    stop("sort parameter must equal \"alpha\", \"ascending\", or \"descending\"")
  }
  sorted$item <- factor(sorted$item, ordered=T, levels=sorted$item)
  return(ggplot(sorted, aes(x=item, y=Freq)) +
           geom_bar(stat="identity") +
           xlab("") +
           ylab("Count") +
           coord_flip() +
           theme)
}