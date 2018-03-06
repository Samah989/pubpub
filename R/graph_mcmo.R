#' Graphs a multiple choice, multiple option question
#' 
#' This function creates a horizontal bar graph for a multiple choice, multiple option question
#' @export
graph_mcmo <- function(item, sort="alpha", custom=NULL, stat="count", fill="seagreen", ...) {
  dummies <- create_mcmodummies(item, ...)
  names <- wrap_strings(names(dummies), ...)
  if(stat=="count") {
    stats <- colSums(dummies, na.rm=T)
  } else if(stat=="percent") {
    stats <- 100*colMeans(dummies, na.rm=T)
  } else {
    stop("stat parameter must equal \"count\" or \"percent\"")
  }
  allstats <- data.frame(cbind(names, stats))
  names(allstats) <- c("item", "stat")
  allstats$stat <- as.numeric(as.character(allstats$stat))
  rownames(allstats) <- NULL
  if(sort=="descending") {
    allstats <- allstats[order(allstats$stat),]
    sorder <- allstats$item
  } else if(sort=="ascending") {
    allstats <- allstats[order(-allstats$stat),]
    sorder <- allstats$item
  } else if(sort=="alpha") {
    allstats <- allstats[order(allstats$item),]
    allstats <- allstats[rev(1:nrow(allstats)),]
    sorder <- allstats$item
  } else if(sort=="custom") {
    sorder <- custom[rev(1:length(custom))]
  } else {
    stop("sort parameter must equal \"alpha\", \"ascending\", \"descending\", or \"custom\"")
  }
  allstats$item <- factor(allstats$item, ordered=T, levels=sorder)
  plot <- ggplot(allstats, aes(x=item, y=stat)) +
    geom_bar(stat="identity", fill=fill) +
    xlab("")
  if(stat=="count") {
    plot <- plot +
      ylab("Count")
  } else if(stat=="percent") {
    plot <- plot +
      ylab("Percent")
  }
  plot <- plot +
    coord_flip() +
    theme(panel.background=element_blank(),
          axis.line=element_line(colour="black"))
  return(plot)
}