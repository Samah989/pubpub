#' Graphs a multiple choice, single option question
#' 
#' This function creates a horizontal bar graph gor a multiple choice, single option question
graph_mcso <- function(item, sort="alpha") {
  data <- data.frame(item)
  sorting <- data.frame(table(data))
  if(sort=="descending") {
    sorting <- sorting[order(sorting$Freq),]
  } else if(sort=="ascending") {
    sorting <- sorting[order(-sorting$Freq),]
  } else if(sort=="alpha") {
    sorting <- sorting[order(sorting$Var1),]
    sorting <- sorting[rev(1:nrow(sorting)),]
  } else {
    stop("sort parameter must equal \"alpha\", \"ascending\", or \"descending\"")
  }
  data$q2 <- factor(data$q2, ordered=T, levels=sorting[,1])
  return(ggplot(data, aes(x=item)) +
    geom_bar(stat="count") +
    xlab("") +
    ylab("Count") +
    coord_flip() +
    theme)
}