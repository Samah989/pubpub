#' Graphs multiple numeric questions
#' 
#' Creates plots for numeric questions where their distributions are meant to
#' be comparable to each other (e.g., where they are supposed to sum to 100)
#' 
#' @param items Data frame with only items to be graphed
#' @param labels Vector, in order of \code{items} columns, with names of items
#' @param sort The order in which items should be displayed upon graphing. 
#' Options include: \code{entry} - options are in the order of the 
#' original columns, and \code{descending} - options are in descending order
#' by the chosen \code{central} statistic
#' @param totalcheck Includes a value for if the user wants to omit rows for
#' which the constituent items do not correctly add up to some total value
#' @param central For horizontal bar graphs, specifies the statistic used for
#' the height of the bars. Options include: \code{mean} for the arithmetic
#' mean, and \code{median} for the median
#' @param spread For horizontal bar graphs, specifies the statistic used for
#' the endpoints of the error bars. Options include: \code{sd1} for a width
#' of one standard deviation, \code{sd2} for a width of 1.96 standard
#' deviations, and \code{minmax} for a width using the range of the data
#' @param fill Specifies the color of the bars in the graph, see 
#' <http://sape.inf.usi.ch/quick-reference/ggplot2/colour> for color
#' references
#' 
#' @examples
#' # Graph all time use questions
#' graph_linkednum(survey[get_matchvars(survey, "TimeUse_")], 
#'                        labels=c("Talking", "Eating", "Drinking"),
#'                        totalcheck=100)
#' 
#' @export
graph_linkednum <- function(items, labels, sort="entry", totalcheck=NA, 
                            central="mean", spread="sd1", fill="tomato1") {
  orig <- nrow(items)
  totalcheck <- 100
  if(!is.na(totalcheck)) {
    items$check <- rowSums(items)
    items <- items[!is.na(items$check),]
    items <- items[items$check==totalcheck,]
    items$check <- NULL
    newlen <- nrow(items)
    if((orig-newlen)>0) {
      warning(paste0((orig-newlen), " rows do not add to ", totalcheck, "."), call.=F)
    }
  }
  sumtable <- data.frame(lapply(items, function(x) rbind(mean=mean(x),
                                                         sd=sd(x),
                                                         median=median(x),
                                                         minimum=min(x),
                                                         maximum=max(x))))
  sumtable <- data.frame(t(sumtable))
  sumtable$item <- row.names(sumtable)
  map <- hashmap(names(items), labels)
  sumtable$item <- map[[sumtable$item]]
  if(central=="mean") {
    sumtable$central <- sumtable$mean
    type <- "Mean"
  } else if(central=="median") {
    sumtable$central <- sumtable$median
    type <- "Median"
  }
  if(spread=="sd1") {
    sumtable$lo <- sumtable$mean-sumtable$sd
    sumtable$lo[sumtable$lo<0] <- 0
    sumtable$hi <- sumtable$mean+sumtable$sd
  } else if(spread=="sd2") {
    sumtable$lo <- sumtable$mean-(1.96*sumtable$sd)
    sumtable$lo[sumtable$lo<0] <- 0
    sumtable$hi <- sumtable$mean+(1.96*sumtable$sd)
  } else if(spread=="minmax") {
    sumtable$lo <- sumtable$min
    sumtable$lo[sumtable$lo<0] <- 0
    sumtable$hi <- sumtable$max
  }
  if(sort=="entry") {
    sumtable$item <- factor(sumtable$item, ordered=T, levels=rev(labels))
  } else if(sort=="descending") {
    sumtable$item <- factor(sumtable$item, ordered=T, levels=sumtable$item[order(sumtable$central)])
  }
  ggplot(sumtable, aes(x=item, y=central)) + 
    geom_bar(stat="identity", fill="tomato1") + 
    geom_errorbar(aes(ymin=lo, ymax=hi), width=.2) +
    xlab("") +
    ylab(paste(type, "Percent")) +
    coord_flip() +
    theme(panel.background=element_blank(),
          axis.line=element_line(colour="black"))
}