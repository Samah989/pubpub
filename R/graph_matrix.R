#' Graphs a set of matrix questions
#' 
#' Graphs a horizontal stacked bar plot to display information from a series
#' of questions that use the same options (e.g., a matrix).
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
#' @param mcmo Indicator for whether respondents could choose more than one
#' option, default is FALSE
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
graph_matrix <- function(items, levels, labels, sort="entry", 
                         mcmo=F, palette="ordered", ...) {
  # Reverses the order of the levels for horizontal printing
  levels <- rev(levels)
  
  # Initializes summary data frame for sorted data
  sorted <- data.frame(matrix(NA, nrow=1, ncol=length(levels)))
  names(sorted) <- levels
  
  # If respondents could select more than one response per row
  if(!mcmo) {
    
    # Turn items into factors with specified levels
    items <- lapply(items, function(x) factor(x, levels=levels))
    
    # Add percentages from proportion tables for each item
    for(i in 1:length(items)) {
      sorted <- rbind(sorted, 100*prop.table(table(items[i])))
    }
  } else {
    
    # Add percentages from summed dummy matrices for each item
    for(i in 1:length(items)) {
      sorted <- rbind(sorted, lapply(create_mcmodummies(items[,i],
                                                        custom.opts=levels),
                                     function(x) sum(x)))
    }
  }
  
  # Clean up sorted summary table
  sorted <- sorted[-1,]
  sorted <- cbind(labels, sorted)
  names(sorted)[1] <- "Item"
  
  # Create item sort based on input
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
  
  # Reshape summary table to long (item x option)
  coldata <- melt(sorted, id="Item")
  
  # Wrap strings in summary table
  coldata$Item <- wrap_strings(as.character(coldata$Item))
  
  # Sort table based on sorted order
  coldata$Item <- factor(coldata$Item, levels=wrap_strings(sorder), ordered=T)
  
  # Specify color based on input
  if(palette=="ordered") {
    palette <- "YlOrRd"
  } else if(palette=="unordered") {
    palette <- "Pastel1"
  } else {
    stop("palette parameter must equal \"ordered\" or \"unordered\"")
  }
  
  # Specify y axis label based on input
  if(!mcmo) {
    ylabel <- "Percent"
  } else {
    ylabel <- "Count"
  }
  
  # Return graph
  return(ggplot(coldata, aes(x=factor(Item), y=value, fill=factor(variable))) + 
           geom_bar(stat="identity") +
           xlab("") +
           ylab(ylabel) +
           coord_flip() + 
           scale_fill_brewer(name="Response", palette=palette) +
           theme(panel.background=element_blank(),
                 axis.line=element_line(colour="black")))
}