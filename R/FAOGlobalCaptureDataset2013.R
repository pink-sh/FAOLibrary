plotQuantity <- function(start=1900, end=2100, file="FAOGlobalCaptureDataset2013.csv") {
    library(rCharts)
    library(dplyr)
    myData <- read.csv(file)
    aggr1 <- aggregate(myData$QUANTITY, by=list(YR_ITEM=myData$YR_ITEM), FUN=sum)
    aggr12 <- transform(aggr1, YR_ITEM = as.character(YR_ITEM), Quantity = as.numeric(x))
    aggr13 <- filter(aggr12, YR_ITEM >= start, YR_ITEM <= end)
    m1 <- mPlot(x = "YR_ITEM", y = c("x"), type = "Line", data = aggr13)
    m1$save('output.html', standalone = TRUE)
}

plotQuantityBySpecies <- function(species="ALR", start=1900, end=2100, file="FAOGlobalCaptureDataset2013.csv") {
  library(rCharts)
  library(dplyr)
  myData <- read.csv(file)
  aggr0 <- filter(myData, ALPHA3CODE == species)
  aggr1 <- aggregate(aggr0$QUANTITY, by=list(YR_ITEM=aggr0$YR_ITEM), FUN=sum)
  aggr12 <- transform(aggr1, YR_ITEM = as.character(YR_ITEM), Quantity = as.numeric(x))
  aggr13 <- filter(aggr12, YR_ITEM >= start, YR_ITEM <= end)
  m1 <- mPlot(x = "YR_ITEM", y = c("x"), type = "Line", data = aggr13)
  m1$save('output.html', standalone = TRUE)
}

getSpecies <- function(file="FAOGlobalCaptureDataset2013.csv") {
  library(jsonlite)
  library(plyr)
  df <- read.csv(file)
  plyed <- ddply(df, c("FIC_ITEM_NAME_E","ALPHA3CODE"), head, 1)
  ret <- data.frame("NAME" = plyed$FIC_ITEM_NAME_E, "ALPHA" = plyed$ALPHA3CODE)
  ret <- ret[!apply(ret, 1, function(x) any(x=="")),]
  return (toJSON(ret))
}
