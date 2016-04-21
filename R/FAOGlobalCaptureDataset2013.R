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

plotQuantityByMultipleSpecies <- function(species=c("ALR","LAS", "TUC"), start=1946, end=2016, file="FAOGlobalCaptureDataset2013.csv") {
  library(rCharts)
  library(dplyr)
  myData <- read.csv(file)
  reduced <- filter(myData, ALPHA3CODE %in% species)
  OUT <- data.frame("YR_ITEM"=(start:end))
  for (sp in species) {
    aggr0 <- filter(reduced, ALPHA3CODE == sp)
    aggr01 <- aggregate(aggr0$QUANTITY, by=list(YR_ITEM=aggr0$YR_ITEM), FUN=sum)
    aggr02 <- transform(aggr01, YR_ITEM = as.character(YR_ITEM), Quantity = as.numeric(x))
    aggr03 <- filter(aggr02, YR_ITEM >= start, YR_ITEM <= end)
    aggr03$x <- NULL
    vector <- c()
    i = 1;
    apply(OUT, 1, function(row1) {
      yrOut = row1['YR_ITEM']
      value <- 0
      apply(aggr03, 1, function(row2) {
        yrIn = row2['YR_ITEM']
        if (!is.na(yrIn)) {
          if (yrOut == yrIn) {
            value <<- row2['Quantity']
          }
        }
      })
      vector <<- c(vector, value)
      i <<- i + 1
    })
    OUT[[sp]] <- vector
  }
  OUT <- transform(OUT, YR_ITEM = as.character(YR_ITEM))
  m1 <- mPlot(x = "YR_ITEM", y = species, type = "Line", data = OUT)
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

getSpeciesByLocale <- function(file="FAOGlobalCaptureDataset2013.csv", locale="EN") {
  library(jsonlite)
  library(plyr)
  df <- read.csv(file)
  species <- read.csv("/usr/local/opencpu/ASFIS_6_languages_160420_utf8.csv", sep = "\t", encoding="UTF-8")
  plyed <- ddply(df, c("ALPHA3CODE"), head, 1)
  plyedWithNames <- ddply(df, c("ALPHA3CODE", "FIC_ITEM_NAME_E"), head, 1)
  ALPHA3 <- data.frame("ALPHA" = plyed$ALPHA3CODE)
  ALPHA3 <- ALPHA3[!apply(ALPHA3, 1, function(x) any(x=="")),]
  NAME <- c()
  for (val in ALPHA3) {
    row <- lapply(subset(species, X3A_CODE==val), as.character)
    nm <- row$Scientific_name

    if (identical(nm, character(0))) {
      rowOrig <- lapply(subset(plyedWithNames, ALPHA3CODE==val), as.character)
      nm <- rowOrig$FIC_ITEM_NAME_E
    } else if (locale == "EN" && row$English_name != "") {
      nm <- row$English_name
    } else if (locale == "ES" && row$Spanish_name != "") {
      nm <- row$Spanish_name
    } else if (locale == "FR" && row$French_name != "") {
      nm <- row$French_name
    } else if (locale == "AR" && row$Arabic_name != "") {
      nm <- row$Arabic_name
    } else if (locale == "ZH" && row$Chinese_name != "") {
      nm <- row$Chinese_name
    } else if (locale == "RU" && row$Russian_name != "") {
      nm <- row$Russian_name
    }
    NAME <- c(NAME, nm)
  }
  ret <- data.frame(ALPHA3, NAME)
  return (toJSON(ret))
}
