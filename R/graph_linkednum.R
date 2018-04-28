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

# graph_linkednum <- function(items, labels, sort="entry", totalcheck=NA, 
#                             central="mean", spread="sd1", fill="tomato1") {
#   orig <- nrow(items)
#   totalcheck <- 100
#   if(!is.na(totalcheck)) {
#     items$check <- rowSums(items)
#     items <- items[!is.na(items$check),]
#     items <- items[items$check==totalcheck,]
#     items$check <- NULL
#     newlen <- nrow(items)
#     if((orig-newlen)>0) {
#       warning(paste0((orig-newlen), " rows do not add to ", totalcheck, "."), call.=F)
#     }
#   }
#   sumtable <- data.frame(lapply(items, function(x) rbind(mean=mean(x),
#                                                          sd=sd(x),
#                                                          median=median(x),
#                                                          minimum=min(x),
#                                                          maximum=max(x))))
#   sumtable <- data.frame(t(sumtable))
#   sumtable$item <- row.names(sumtable)
#   map <- hashmap(names(items), labels)
#   sumtable$item <- map[[sumtable$item]]
#   if(central=="mean") {
#     sumtable$central <- sumtable$mean
#     type <- "Mean"
#   }
#   if(spread=="sd1") {
#     sumtable$lo <- sumtable$mean-sumtable$sd
#     sumtable$lo[sumtable$lo<0] <- 0
#     sumtable$hi <- sumtable$mean+sumtable$sd
#   }
#   if(sort=="entry") {
#     sumtable$item <- factor(sumtable$item, ordered=T, levels=rev(labels))
#   } else if(sort=="descending") {
#     sumtable$item <- factor(sumtable$item, ordered=T, levels=sumtable$item[order(sumtable$mean)])
#   }
#   return(ggplot(sumtable, aes(x=item, y=central)) + 
#     geom_bar(stat="identity", fill="tomato1") + 
#     geom_errorbar(aes(ymin=lo, ymax=hi), width=.2) +
#     xlab("") +
#     ylab(paste(type, "Percent")) +
#     coord_flip() +
#     theme(panel.background=element_blank(),
#           axis.line=element_line(colour="black")))
# }
# 
# timevars <- OurVar[,c("TimeGatheringData", "TimeModelBuilding", "TimeProduction", "TimeVisualizing", "TimeFindingInsights", "TimeOtherSelect")]
# 
# 
# graph_linkednum(timevars, 
#                 labels=c("Gathering data", "Model building", "Production", "Visualizing", "Finding insights", "Other"), 
#                 sort="descending",
#                 totalcheck=100,
#                 central="mean", spread="sd1", fill="tomato1")
# 
# 
# timevars$total <- NULL
# timevars$id <- NULL
# labels <- c("Gathering data", "Model building", "Production", "Visualizing", "Finding insights", "Other")
# 
# orig <- nrow(timevars)
# totalcheck <- 100
# if(!is.na(totalcheck)) {
#   timevars$check <- rowSums(timevars)
#   timevars <- timevars[!is.na(timevars$check),]
#   timevars <- timevars[timevars$check==totalcheck,]
#   timevars$check <- NULL
#   newlen <- nrow(timevars)
#   if((orig-newlen)>0) {
#     warning(paste0((orig-newlen), " rows do not add to ", totalcheck, "."), call.=F)
#   }
# }
# library(hashmap)
# 
# means <- colMeans(timevars, na.rm=T)
# order1 <- names(means)[order(means)]
# order2 <- labels[order(means)]
# 
# map <- hashmap(order1, order2)
# 
# 
# timevars <- timevars[order(timevars[,order1[length(order1)]]),]
# 
# separator <- nrow(timevars)
# 
# meansbar <- data.frame(matrix(rep(means, 50), byrow=T, nrow=50, ncol=length(means)))
# names(meansbar) <- names(means)
# timevars <- rbind(timevars, meansbar)
# 
# 
# timevars$id <- 1:nrow(timevars)
# library(ggplot2)
# library(tidyr)
# timevars2 <- timevars %>% gather(item, value, -c(id))
# timevars2 <- timevars2[order(timevars2$id),]
# 
# 
# timevars2$item <- map[[timevars2$item]]
# timevars2$item <- factor(timevars2$item, ordered=T,
#                          levels=order2)
# 
# ggplot(timevars2, aes(id, value)) +
#   geom_area(aes(fill=item), position = 'stack') +
#   geom_vline(xintercept=separator, size=1) + 
#   annotate("text", x=mean(c(separator, nrow(timevars))), y=0.05*totalcheck, label="Averages") +
#   xlab(paste0("Individuals (ordered by \"", order[length(order)], "\")")) +
#   ylab("Percent") +
#   scale_fill_brewer(name="", palette="Pastel1") +
#   theme(panel.background=element_blank(),
#         axis.line=element_line(colour="black"),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
# 
# timevars$id <- NULL
# 
# res <- data.frame(lapply(timevars , function(x) rbind( mean = mean(x) ,
#                                          sd = sd(x) ,
#                                          median = median(x) ,
#                                          minimum = min(x) ,
#                                          maximum = max(x))))
# res <- data.frame(t(res))
# res$item <- row.names(res)
# res$item <- map[[res$item]]
# res$lo <- res$mean-res$sd
# res$lo[res$lo<0] <- 0
# res$hi <- res$mean+res$sd
# res$item <- factor(res$item, ordered=T, levels=res$item[order(res$mean)])
# 
# ggplot(res, aes(x=item, y=mean)) + 
#   geom_bar(stat="identity", fill="tomato1") + 
#   geom_errorbar(aes(ymin=lo, ymax=hi), width=.2) +
#   xlab("") +
#   ylab("Percent") +
#   coord_flip() +
#   theme(panel.background=element_blank(),
#         axis.line=element_line(colour="black"))