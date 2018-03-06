#' Graphs multiple numeric questions
#' 
#' This function creates bar plots with error bars for linked numeric questions
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
    sumtable$item <- factor(sumtable$item, ordered=T, levels=sumtable$item[order(sumtable$mean)])
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