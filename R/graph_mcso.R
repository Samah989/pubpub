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
    sorted <- sorted[order(sorted$Var1),]
    sorted <- sorted[rev(1:nrow(sorted)),]
  } else {
    stop("sort parameter must equal \"alpha\", \"ascending\", or \"descending\"")
  }
  sorted$Var1 <- factor(sorted$Var1, ordered=T, levels=sorted$Var1)
  return(ggplot(sorted, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity") +
    xlab("") +
    ylab("Count") +
    coord_flip() +
    theme)
}