#' Graphs a set of matrix questions
#' 
#' Graphs a horizontal stacked bar plot to display information from a series
#' of questions that use the same options (e.g., a matrix)
#' 
#' @param items Data frame with only items to be graphed
#' @param levels Vector with the order (low to high) of ordinal options, must
#' be specified even if options are not ordinal
#' @param labels Vector, in order of \code{items} columns, with names of items
#' @param sort The order in which items should be displayed upon graphing. 
#' Options include: \code{entry} - options are in the order of the 
#' original columns, \code{alpha} - options are in alphabetical order, or one
#' of the levels from \code{levels}, in which case the graph will be sorted
#' descending order by that level
#' @param palette Specifies the if the levels are ordered or not, if 
#' "ordered," palette is "YlOrRd", if "unordered," palette is "Pastel1" - see
#' <http://ggplot2.tidyverse.org/reference/scale_brewer.html> for more detail
#' 
#' @examples 
#' # Graph agree/disagree questions
#' graph_matrix(survey[get_matchvars(survey, "Opinion_")],
#'                     levels=c("Disagree", "Neutral", "Agree"),
#'                     labels=c("Apples", "Bananas", "Coconuts"),
#'                     sort="Agree")
#' 
#' @export
graph_matrix <- function(items, levels, labels, sort="entry", palette="ordered", ...) {
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
  if(palette=="ordered") {
    palette <- "YlOrRd"
  } else if(palette=="unordered") {
    palette <- "Pastel1"
  } else {
    stop("palette parameter must equal \"ordered\" or \"unordered\"")
  }
  return(ggplot(coldata, aes(x=factor(Item), y=value, fill=factor(variable))) + 
           geom_bar(stat="identity") +
           xlab("") +
           ylab("Percent") +
           coord_flip() + 
           scale_fill_brewer(name="Response", palette=palette) +
           theme(panel.background=element_blank(),
                 axis.line=element_line(colour="black")))
}