#' Graphs an MCMO question
#' 
#' This function creates a horizontal bar graph for an MCMO question
#' 
#' @param item Data vector from MCMO question
#' @param sort Specifies the order of the items in the graph. Options include:
#' \code{descending} - in descending order by frequency, \code{ascending} - in
#' ascending order by frequency, \code{alpha} - in alphabetical order,
#' \code{custom} - in some custom order specified in the \code{custom} parameter
#' @param custom If \code{sort="custom"}, the order of the options to be graphed
#' @param stat The statistic for the frequency, either \code{count} or
#' \code{percent}
#' @param fill Specifies the color of the bars in the graph, see 
#' <http://sape.inf.usi.ch/quick-reference/ggplot2/colour> for color
#' references
#' 
#' @examples 
#' # Graphs an MCMO question
#' graph_mcmo(survey$Q3, sort="custom", custom=c("Grains", "Fruits", "Vegetables"))
#' 
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