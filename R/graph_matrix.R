#' Graphs a multiple choice, single option question
#' 
#' This function creates a horizontal bar graph for a multiple choice, single option question
#' @export
graph_matrix <- function(items, levels, labels, sort="entry", ...) {
  levels <- rev(levels)
  items <- lapply(items, function(x) factor(x, levels=levels))
  sorted <- data.frame(matrix(NA, nrow=1, ncol=length(levels)))
  names(sorted) <- levels
  for(i in 1:length(items)) {
    sorted <- rbind(sorted, 100*prop.table(table(items[i])))
  }
  sorted <- sorted[-1,]
  sorted <- cbind(labels, sorted)
  names(sorted)[1] <- "Item"
  if(sort=="entry") {
    sorder <- rev(labels)
  } else if(sort=="alpha") {
    sorder <- labels[order(labels)]
    sorder <- rev(sorder)
  } else if(sort %in% levels) {
    sorted <- sorted[order(sorted[sort]),]
    sorder <- as.character(sorted$Item)
  } else {
    stop("sort parameter must equal \"entry\", \"alpha\", or one of the item options")
  }
  coldata <- melt(sorted, id="Item")
  coldata$Item <- wrap_strings(as.character(coldata$Item))
  coldata$Item <- factor(coldata$Item, levels=wrap_strings(sorder), ordered=T)
  return(ggplot(coldata, aes(x=factor(Item), y=value, fill=factor(variable))) + 
           geom_bar(stat="identity") +
           xlab("") +
           ylab("Percent") +
           coord_flip() + 
           scale_fill_brewer(name="Response", ...) +
           theme)
}