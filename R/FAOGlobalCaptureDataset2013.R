library(rCharts)
library(dplyr)
plotQuantity <- function(start=1900, end=2100, file="FAOGlobalCaptureDataset2013.csv") {
  myData <- read.csv(file)
  aggr1 <- aggregate(myData$QUANTITY, by=list(YR_ITEM=myData$YR_ITEM), FUN=sum)
  aggr12 <- transform(aggr1, YR_ITEM = as.character(YR_ITEM), Quantity = as.numeric(x))
  aggr13 <- filter(aggr12, YR_ITEM >= start, YR_ITEM <= end)
  lines(aggr13)
  m1 <- mPlot(x = "YR_ITEM", y = c("x"), type = "Line", data = aggr13)
  m1$save('/home/enrico/Work/R/mychart3.html', standalone = TRUE)
}
